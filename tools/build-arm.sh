#!/bin/sh

# Spin up an ARM build machine using aws cli, build pandoc, and
# download the artifact.
#
# We need to use us-east-2; since my us-west-1 has EC2-classic.
# docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-classic-platform.html

aws configure set default.region us-east-2

# Now start instance with volume 16GB.
# note, the AMI id is different for different regions.
# for us east-2 the Debian Buster ARM AMI is  ami-0fa8979d18f69948b:

echo "Creating instance..."

aws ec2 run-instances --image-id ami-0fa8979d18f69948b  --count 1 --instance-type t4g.2xlarge --block-device-mapping 'DeviceName=/dev/xvda,Ebs={VolumeSize=16}' --key-name debian-arm-us-east-2 --security-group-ids sg-086ffbadc286c5c00 > ec2.json

# Now get the public IP address.

INSTANCEID=$(jq '.Instances[0].InstanceId' ec2.json | sed -e 's/"//g')
IPADDR=$(aws ec2 describe-instances --instance-ids="$INSTANCEID" --query 'Reservations[0].Instances[0].PublicIpAddress' | sed -e 's/"//g')

clean_up() {
  echo "Terminating the instance..."
  aws ec2 terminate-instances --instance-ids "$INSTANCEID"
}
trap clean_up EXIT

echo "Waiting for instance to start up..."

STATUS=none
while [ "$STATUS" != "running" ]
do
  sleep 20
  STATUS=$(aws ec2 describe-instance-status --instance-id "$INSTANCEID" | jq '.InstanceStatuses[0].InstanceState.Name' | sed -e 's/"//g')
  echo "...$STATUS"
done

# At this point you can connect via SSH, or run this script:
# $ ssh -i ~/.ssh/debian-arm-us-east-2.pem admin@$IPADDR

SSH="ssh -i ~/.ssh/debian-arm-us-east-2.pem admin@$IPADDR"
SCP="scp -r -i ~/.ssh/debian-arm-us-east-2.pem admin@$IPADDR:"

echo "Provisioning..."

$SSH <<EOF
sudo apt-get update
sudo apt-get upgrade -y
sudo apt-get install -y apt-transport-https  ca-certificates curl gnupg git make
curl -fsSL https://download.docker.com/linux/debian/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
echo \
  "deb [arch=arm64 signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/debian \
  \$(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list
sudo apt-get update
sudo apt-get install -y docker-ce docker-ce-cli containerd.io
sudo groupadd docker
sudo usermod -aG docker admin
EOF

echo "Building..."

($SSH <<EOF
mkdir src
cd src || exit
git clone https://github.com/jgm/pandoc
cd pandoc || exit
make debpkg
EOF
) &

# Now we need to wait for the build to complete (this can take 3 hours).

while true
do
  sleep 60
  $SSH "tail -n1 src/pandoc/docker.log && free -h"
  # Check to see if the artifact has been produced
  $SSH "ls -l linux/artifacts/*.tar.gz" && break
done

# Retrieve the artifacts

echo "Successful build. Retrieving artifacts..."

$SCP -r src/pandoc/linux/artifacts "arm-build-artifacts-$(date +%s)"

exit 0
