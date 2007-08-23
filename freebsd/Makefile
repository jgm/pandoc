# New ports collection makefile for: pandoc
# Date created:                      9 February 2007
# Whom:                              John MacFarlane <jgm@berkeley.edu>
#
# $FreeBSD: ports/textproc/pandoc/Makefile,v 1.2 2007/05/19 20:24:07 flz Exp $
#

PORTNAME=	pandoc
PORTVERSION=	0.42
CATEGORIES=	textproc haskell
MASTER_SITES=	http://pandoc.googlecode.com/files/
DISTNAME=	pandoc-${PORTVERSION}

MAINTAINER=	jgm@berkeley.edu
COMMENT=	A general markup converter

BUILD_DEPENDS=	ghc>=6.6:${PORTSDIR}/lang/ghc

MANCOMPRESSED=	no
MAN1=		pandoc.1 markdown2pdf.1 html2markdown.1 hsmarkdown.1

USE_GMAKE=	yes
USE_PERL5=	yes

PLIST_FILES=	bin/pandoc bin/markdown2pdf bin/html2markdown bin/hsmarkdown
PORTDOCS=	BUGS README README.html
SCRIPTS=	hsmarkdown html2markdown markdown2pdf

do-install:
	@${INSTALL_PROGRAM} ${WRKSRC}/dist/build/pandoc/pandoc ${PREFIX}/bin
.for script in ${SCRIPTS}
	@${INSTALL_SCRIPT} ${WRKSRC}/${script} ${PREFIX}/bin
.endfor
.for man in ${MAN1}
	@${INSTALL_MAN} ${WRKSRC}/man/man1/${man} ${PREFIX}/man/man1
.endfor
.if !defined(NOPORTDOCS)
	@${MKDIR} ${DOCSDIR}
.for doc in ${PORTDOCS}
	@${INSTALL_DATA} ${WRKSRC}/${doc} ${DOCSDIR}
.endfor
.endif

.include <bsd.port.mk>
