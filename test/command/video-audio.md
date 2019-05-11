```
% pandoc -f markdown-implicit_figures -t html
![](./test.mp4)

![Your browser does not support video.](foo/test.webm){width=300}

![](test.mp3)

![](./test.pdf)

![](./test.jpg)
^D
<p><video src="./test.mp4" controls=""><a href="./test.mp4">Video</a></video></p>
<p><video src="foo/test.webm" width="300" controls=""><a href="foo/test.webm">Your browser does not support video.</a></video></p>
<p><audio src="test.mp3" controls=""><a href="test.mp3">Audio</a></audio></p>
<p><embed src="./test.pdf" /></p>
<p><img src="./test.jpg" /></p>
```

