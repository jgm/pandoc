{-# LANGUAGE NoImplicitPrelude #-}
{- |
   Module      : Text.Pandoc.Asciify
   Copyright   : Copyright (C) 2013-2019 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Function to convert accented latin letters to their unaccented
ascii equivalents (used in constructing HTML identifiers).
-}
module Text.Pandoc.Asciify (toAsciiChar)
where
import Prelude
import Data.Char (isAscii)
import qualified Data.Map as M

toAsciiChar :: Char -> Maybe Char
toAsciiChar c | isAscii c = Just c
              | otherwise = M.lookup c asciiMap

asciiMap :: M.Map Char Char
asciiMap = M.fromList
  [('\192','A')
  ,('\193','A')
  ,('\194','A')
  ,('\195','A')
  ,('\196','A')
  ,('\197','A')
  ,('\199','C')
  ,('\200','E')
  ,('\201','E')
  ,('\202','E')
  ,('\203','E')
  ,('\204','I')
  ,('\205','I')
  ,('\206','I')
  ,('\207','I')
  ,('\209','N')
  ,('\210','O')
  ,('\211','O')
  ,('\212','O')
  ,('\213','O')
  ,('\214','O')
  ,('\217','U')
  ,('\218','U')
  ,('\219','U')
  ,('\220','U')
  ,('\221','Y')
  ,('\224','a')
  ,('\225','a')
  ,('\226','a')
  ,('\227','a')
  ,('\228','a')
  ,('\229','a')
  ,('\231','c')
  ,('\232','e')
  ,('\233','e')
  ,('\234','e')
  ,('\235','e')
  ,('\236','i')
  ,('\237','i')
  ,('\238','i')
  ,('\239','i')
  ,('\241','n')
  ,('\242','o')
  ,('\243','o')
  ,('\244','o')
  ,('\245','o')
  ,('\246','o')
  ,('\249','u')
  ,('\250','u')
  ,('\251','u')
  ,('\252','u')
  ,('\253','y')
  ,('\255','y')
  ,('\256','A')
  ,('\257','a')
  ,('\258','A')
  ,('\259','a')
  ,('\260','A')
  ,('\261','a')
  ,('\262','C')
  ,('\263','c')
  ,('\264','C')
  ,('\265','c')
  ,('\266','C')
  ,('\267','c')
  ,('\268','C')
  ,('\269','c')
  ,('\270','D')
  ,('\271','d')
  ,('\274','E')
  ,('\275','e')
  ,('\276','E')
  ,('\277','e')
  ,('\278','E')
  ,('\279','e')
  ,('\280','E')
  ,('\281','e')
  ,('\282','E')
  ,('\283','e')
  ,('\284','G')
  ,('\285','g')
  ,('\286','G')
  ,('\287','g')
  ,('\288','G')
  ,('\289','g')
  ,('\290','G')
  ,('\291','g')
  ,('\292','H')
  ,('\293','h')
  ,('\296','I')
  ,('\297','i')
  ,('\298','I')
  ,('\299','i')
  ,('\300','I')
  ,('\301','i')
  ,('\302','I')
  ,('\303','i')
  ,('\304','I')
  ,('\305','i')
  ,('\308','J')
  ,('\309','j')
  ,('\310','K')
  ,('\311','k')
  ,('\313','L')
  ,('\314','l')
  ,('\315','L')
  ,('\316','l')
  ,('\317','L')
  ,('\318','l')
  ,('\323','N')
  ,('\324','n')
  ,('\325','N')
  ,('\326','n')
  ,('\327','N')
  ,('\328','n')
  ,('\332','O')
  ,('\333','o')
  ,('\334','O')
  ,('\335','o')
  ,('\336','O')
  ,('\337','o')
  ,('\340','R')
  ,('\341','r')
  ,('\342','R')
  ,('\343','r')
  ,('\344','R')
  ,('\345','r')
  ,('\346','S')
  ,('\347','s')
  ,('\348','S')
  ,('\349','s')
  ,('\350','S')
  ,('\351','s')
  ,('\352','S')
  ,('\353','s')
  ,('\354','T')
  ,('\355','t')
  ,('\356','T')
  ,('\357','t')
  ,('\360','U')
  ,('\361','u')
  ,('\362','U')
  ,('\363','u')
  ,('\364','U')
  ,('\365','u')
  ,('\366','U')
  ,('\367','u')
  ,('\368','U')
  ,('\369','u')
  ,('\370','U')
  ,('\371','u')
  ,('\372','W')
  ,('\373','w')
  ,('\374','Y')
  ,('\375','y')
  ,('\376','Y')
  ,('\377','Z')
  ,('\378','z')
  ,('\379','Z')
  ,('\380','z')
  ,('\381','Z')
  ,('\382','z')
  ,('\416','O')
  ,('\417','o')
  ,('\431','U')
  ,('\432','u')
  ,('\461','A')
  ,('\462','a')
  ,('\463','I')
  ,('\464','i')
  ,('\465','O')
  ,('\466','o')
  ,('\467','U')
  ,('\468','u')
  ,('\486','G')
  ,('\487','g')
  ,('\488','K')
  ,('\489','k')
  ,('\490','O')
  ,('\491','o')
  ,('\496','j')
  ,('\500','G')
  ,('\501','g')
  ,('\504','N')
  ,('\505','n')
  ,('\512','A')
  ,('\513','a')
  ,('\514','A')
  ,('\515','a')
  ,('\516','E')
  ,('\517','e')
  ,('\518','E')
  ,('\519','e')
  ,('\520','I')
  ,('\521','i')
  ,('\522','I')
  ,('\523','i')
  ,('\524','O')
  ,('\525','o')
  ,('\526','O')
  ,('\527','o')
  ,('\528','R')
  ,('\529','r')
  ,('\530','R')
  ,('\531','r')
  ,('\532','U')
  ,('\533','u')
  ,('\534','U')
  ,('\535','u')
  ,('\536','S')
  ,('\537','s')
  ,('\538','T')
  ,('\539','t')
  ,('\542','H')
  ,('\543','h')
  ,('\550','A')
  ,('\551','a')
  ,('\552','E')
  ,('\553','e')
  ,('\558','O')
  ,('\559','o')
  ,('\562','Y')
  ,('\563','y')
  ,('\894',';')
  ,('\7680','A')
  ,('\7681','a')
  ,('\7682','B')
  ,('\7683','b')
  ,('\7684','B')
  ,('\7685','b')
  ,('\7686','B')
  ,('\7687','b')
  ,('\7690','D')
  ,('\7691','d')
  ,('\7692','D')
  ,('\7693','d')
  ,('\7694','D')
  ,('\7695','d')
  ,('\7696','D')
  ,('\7697','d')
  ,('\7698','D')
  ,('\7699','d')
  ,('\7704','E')
  ,('\7705','e')
  ,('\7706','E')
  ,('\7707','e')
  ,('\7710','F')
  ,('\7711','f')
  ,('\7712','G')
  ,('\7713','g')
  ,('\7714','H')
  ,('\7715','h')
  ,('\7716','H')
  ,('\7717','h')
  ,('\7718','H')
  ,('\7719','h')
  ,('\7720','H')
  ,('\7721','h')
  ,('\7722','H')
  ,('\7723','h')
  ,('\7724','I')
  ,('\7725','i')
  ,('\7728','K')
  ,('\7729','k')
  ,('\7730','K')
  ,('\7731','k')
  ,('\7732','K')
  ,('\7733','k')
  ,('\7734','L')
  ,('\7735','l')
  ,('\7738','L')
  ,('\7739','l')
  ,('\7740','L')
  ,('\7741','l')
  ,('\7742','M')
  ,('\7743','m')
  ,('\7744','M')
  ,('\7745','m')
  ,('\7746','M')
  ,('\7747','m')
  ,('\7748','N')
  ,('\7749','n')
  ,('\7750','N')
  ,('\7751','n')
  ,('\7752','N')
  ,('\7753','n')
  ,('\7754','N')
  ,('\7755','n')
  ,('\7764','P')
  ,('\7765','p')
  ,('\7766','P')
  ,('\7767','p')
  ,('\7768','R')
  ,('\7769','r')
  ,('\7770','R')
  ,('\7771','r')
  ,('\7774','R')
  ,('\7775','r')
  ,('\7776','S')
  ,('\7777','s')
  ,('\7778','S')
  ,('\7779','s')
  ,('\7786','T')
  ,('\7787','t')
  ,('\7788','T')
  ,('\7789','t')
  ,('\7790','T')
  ,('\7791','t')
  ,('\7792','T')
  ,('\7793','t')
  ,('\7794','U')
  ,('\7795','u')
  ,('\7796','U')
  ,('\7797','u')
  ,('\7798','U')
  ,('\7799','u')
  ,('\7804','V')
  ,('\7805','v')
  ,('\7806','V')
  ,('\7807','v')
  ,('\7808','W')
  ,('\7809','w')
  ,('\7810','W')
  ,('\7811','w')
  ,('\7812','W')
  ,('\7813','w')
  ,('\7814','W')
  ,('\7815','w')
  ,('\7816','W')
  ,('\7817','w')
  ,('\7818','X')
  ,('\7819','x')
  ,('\7820','X')
  ,('\7821','x')
  ,('\7822','Y')
  ,('\7823','y')
  ,('\7824','Z')
  ,('\7825','z')
  ,('\7826','Z')
  ,('\7827','z')
  ,('\7828','Z')
  ,('\7829','z')
  ,('\7830','h')
  ,('\7831','t')
  ,('\7832','w')
  ,('\7833','y')
  ,('\7840','A')
  ,('\7841','a')
  ,('\7842','A')
  ,('\7843','a')
  ,('\7864','E')
  ,('\7865','e')
  ,('\7866','E')
  ,('\7867','e')
  ,('\7868','E')
  ,('\7869','e')
  ,('\7880','I')
  ,('\7881','i')
  ,('\7882','I')
  ,('\7883','i')
  ,('\7884','O')
  ,('\7885','o')
  ,('\7886','O')
  ,('\7887','o')
  ,('\7908','U')
  ,('\7909','u')
  ,('\7910','U')
  ,('\7911','u')
  ,('\7922','Y')
  ,('\7923','y')
  ,('\7924','Y')
  ,('\7925','y')
  ,('\7926','Y')
  ,('\7927','y')
  ,('\7928','Y')
  ,('\7929','y')
  ,('\8175','`')
  ,('\8490','K')
  ,('\8800','=')
  ,('\8814','<')
  ,('\8815','>')
  ]
