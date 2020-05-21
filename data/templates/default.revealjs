<!DOCTYPE html>
<html$if(lang)$ lang="$lang$"$endif$$if(dir)$ dir="$dir$"$endif$>
<head>
  <meta charset="utf-8">
  <meta name="generator" content="pandoc">
$for(author-meta)$
  <meta name="author" content="$author-meta$">
$endfor$
$if(date-meta)$
  <meta name="dcterms.date" content="$date-meta$">
$endif$
$if(keywords)$
  <meta name="keywords" content="$for(keywords)$$keywords$$sep$, $endfor$">
$endif$
  <title>$if(title-prefix)$$title-prefix$ – $endif$$pagetitle$</title>
  <meta name="apple-mobile-web-app-capable" content="yes">
  <meta name="apple-mobile-web-app-status-bar-style" content="black-translucent">
  <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no, minimal-ui">
  <link rel="stylesheet" href="$revealjs-url$/dist/reset.css">
  <link rel="stylesheet" href="$revealjs-url$/dist/reveal.css">
  <style>
    $styles.html()$
  </style>
$if(theme)$
  <link rel="stylesheet" href="$revealjs-url$/dist/theme/$theme$.css" id="theme">
$else$
  <link rel="stylesheet" href="$revealjs-url$/dist/theme/black.css" id="theme">
$endif$
$for(css)$
  <link rel="stylesheet" href="$css$"/>
$endfor$
$if(math)$
  $math$
$endif$
$for(header-includes)$
  $header-includes$
$endfor$
</head>
<body>
$for(include-before)$
$include-before$
$endfor$
  <div class="reveal">
    <div class="slides">

$if(title)$
<section id="$idprefix$title-slide"$for(title-slide-attributes/pairs)$ $it.key$="$it.value$"$endfor$>
  <h1 class="title">$title$</h1>
$if(subtitle)$
  <p class="subtitle">$subtitle$</p>
$endif$
$for(author)$
  <p class="author">$author$</p>
$endfor$
$if(date)$
  <p class="date">$date$</p>
$endif$
</section>
$endif$
$if(toc)$
<section id="$idprefix$TOC">
$table-of-contents$
</section>
$endif$

$body$
    </div>
  </div>

  <script src="$revealjs-url$/dist/reveal.js"></script>

  // reveal.js plugins
  <script src="$revealjs-url$/plugin/notes/notes.js"></script>
  <script src="$revealjs-url$/plugin/search/search.js"></script>
  <script src="$revealjs-url$/plugin/zoom/zoom.js"></script>
$if(mathjax)$
  <script src="$revealjs-url$/plugin/math/math.js"></script>
$endif$

  <script>

      // Full list of configuration options available at:
      // https://revealjs.com/config/
      Reveal.initialize({
$if(controls)$
        // Display controls in the bottom right corner
        controls: $controls$,
$endif$
$if(controlsTutorial)$
        // Help the user learn the controls by providing hints, for example by
        // bouncing the down arrow when they first encounter a vertical slide
        controlsTutorial: $controlsTutorial$,
$endif$
$if(controlsLayout)$
        // Determines where controls appear, "edges" or "bottom-right"
        controlsLayout: $controlsLayout$,
$endif$
$if(controlsBackArrows)$
        // Visibility rule for backwards navigation arrows; "faded", "hidden"
        // or "visible"
        controlsBackArrows: $controlsBackArrows$,
$endif$
$if(progress)$
        // Display a presentation progress bar
        progress: $progress$,
$endif$
$if(slideNumber)$
        // Display the page number of the current slide
        slideNumber: $slideNumber$,
$endif$
$if(hash)$
        // Add the current slide number to the URL hash so that reloading the
        // page/copying the URL will return you to the same slide
        hash: $hash$,
$endif$
        // Push each slide change to the browser history
$if(history)$
        history: $history$,
$else$
        history: true,
$endif$
$if(keyboard)$
        // Enable keyboard shortcuts for navigation
        keyboard: $keyboard$,
$endif$
$if(overview)$
        // Enable the slide overview mode
        overview: $overview$,
$endif$
$if(center)$
        // Vertical centering of slides
        center: $center$,
$endif$
$if(touch)$
        // Enables touch navigation on devices with touch input
        touch: $touch$,
$endif$
$if(loop)$
        // Loop the presentation
        loop: $loop$,
$endif$
$if(rtl)$
        // Change the presentation direction to be RTL
        rtl: $rtl$,
$endif$
$if(navigationMode)$
        // see https://revealjs.com/vertical-slides/#navigation-mode
        navigationMode: '$navigationMode$',
$endif$
$if(shuffle)$
        // Randomizes the order of slides each time the presentation loads
        shuffle: $shuffle$,
$endif$
$if(fragments)$
        // Turns fragments on and off globally
        fragments: $fragments$,
$endif$
$if(fragmentInURL)$
        // Flags whether to include the current fragment in the URL,
        // so that reloading brings you to the same fragment position
        fragmentInURL: $fragmentInURL$,
$endif$
$if(embedded)$
        // Flags if the presentation is running in an embedded mode,
        // i.e. contained within a limited portion of the screen
        embedded: $embedded$,
$endif$
$if(help)$
        // Flags if we should show a help overlay when the questionmark
        // key is pressed
        help: $help$,
$endif$
$if(showNotes)$
        // Flags if speaker notes should be visible to all viewers
        showNotes: $showNotes$,
$endif$
$if(autoPlayMedia)$
        // Global override for autoplaying embedded media (video/audio/iframe)
        // - null: Media will only autoplay if data-autoplay is present
        // - true: All media will autoplay, regardless of individual setting
        // - false: No media will autoplay, regardless of individual setting
        autoPlayMedia: $autoPlayMedia$,
$endif$
$if(preloadIframes)$
        // Global override for preloading lazy-loaded iframes
        // - null: Iframes with data-src AND data-preload will be loaded when within
        //   the viewDistance, iframes with only data-src will be loaded when visible
        // - true: All iframes with data-src will be loaded when within the viewDistance
        // - false: All iframes with data-src will be loaded only when visible
        preloadIframes: $preloadIframes$,
$endif$
$if(autoSlide)$
        // Number of milliseconds between automatically proceeding to the
        // next slide, disabled when set to 0, this value can be overwritten
        // by using a data-autoslide attribute on your slides
        autoSlide: $autoSlide$,
$endif$
$if(autoSlideStoppable)$
        // Stop auto-sliding after user input
        autoSlideStoppable: $autoSlideStoppable$,
$endif$
$if(autoSlideMethod)$
        // Use this method for navigation when auto-sliding
        autoSlideMethod: $autoSlideMethod$,
$endif$
$if(defaultTiming)$
        // Specify the average time in seconds that you think you will spend
        // presenting each slide. This is used to show a pacing timer in the
        // speaker view
        defaultTiming: $defaultTiming$,
$endif$
$if(totalTime)$
        // Specify the total time in seconds that is available to
        // present.  If this is set to a nonzero value, the pacing
        // timer will work out the time available for each slide,
        // instead of using the defaultTiming value
        totalTime: $totalTime$,
$endif$
$if(minimumTimePerSlide)$
        // Specify the minimum amount of time you want to allot to
        // each slide, if using the totalTime calculation method.  If
        // the automated time allocation causes slide pacing to fall
        // below this threshold, then you will see an alert in the
        // speaker notes window
        minimumTimePerSlide: $minimumTimePerSlide$,
$endif$
$if(mouseWheel)$
        // Enable slide navigation via mouse wheel
        mouseWheel: $mouseWheel$,
$endif$
$if(rollingLinks)$
        // Apply a 3D roll to links on hover
        rollingLinks: $rollingLinks$,
$endif$
$if(hideInactiveCursor)$
        // Hide cursor if inactive
        hideInactiveCursor: $hideInactiveCursor$,
$endif$
$if(hideCursorTime)$
        // Time before the cursor is hidden (in ms)
        hideCursorTime: $hideCursorTime$,
$endif$
$if(hideAddressBar)$
        // Hides the address bar on mobile devices
        hideAddressBar: $hideAddressBar$,
$endif$
$if(previewLinks)$
        // Opens links in an iframe preview overlay
        previewLinks: $previewLinks$,
$endif$
$if(transition)$
        // Transition style
        transition: '$transition$', // none/fade/slide/convex/concave/zoom
$endif$
$if(transitionSpeed)$
        // Transition speed
        transitionSpeed: '$transitionSpeed$', // default/fast/slow
$endif$
$if(backgroundTransition)$
        // Transition style for full page slide backgrounds
        backgroundTransition: '$backgroundTransition$', // none/fade/slide/convex/concave/zoom
$endif$
$if(viewDistance)$
        // Number of slides away from the current that are visible
        viewDistance: $viewDistance$,
$endif$
$if(mobileViewDistance)$
        // Number of slides away from the current that are visible on mobile
        // devices. It is advisable to set this to a lower number than
        // viewDistance in order to save resources.
        mobileViewDistance: $mobileViewDistance$,
$endif$
$if(parallaxBackgroundImage)$
        // Parallax background image
        parallaxBackgroundImage: '$parallaxBackgroundImage$', // e.g. "'https://s3.amazonaws.com/hakim-static/reveal-js/reveal-parallax-1.jpg'"
$else$
$if(background-image)$
       // Parallax background image
       parallaxBackgroundImage: '$background-image$', // e.g. "'https://s3.amazonaws.com/hakim-static/reveal-js/reveal-parallax-1.jpg'"
$endif$
$endif$
$if(parallaxBackgroundSize)$
        // Parallax background size
        parallaxBackgroundSize: '$parallaxBackgroundSize$', // CSS syntax, e.g. "2100px 900px"
$endif$
$if(parallaxBackgroundHorizontal)$
        // Amount to move parallax background (horizontal and vertical) on slide change
        // Number, e.g. 100
        parallaxBackgroundHorizontal: $parallaxBackgroundHorizontal$,
$endif$
$if(parallaxBackgroundVertical)$
        parallaxBackgroundVertical: $parallaxBackgroundVertical$,
$endif$
$if(width)$
        // The "normal" size of the presentation, aspect ratio will be preserved
        // when the presentation is scaled to fit different resolutions. Can be
        // specified using percentage units.
        width: $width$,
$endif$
$if(height)$
        height: $height$,
$endif$
$if(margin)$
        // Factor of the display size that should remain empty around the content
        margin: $margin$,
$endif$
$if(minScale)$
        // Bounds for smallest/largest possible scale to apply to content
        minScale: $minScale$,
$endif$
$if(maxScale)$
        maxScale: $maxScale$,
$endif$
$if(zoomKey)$
        // Modifier key used to click-zoom to part of the slide
        zoomKey: '$zoomKey$',
$endif$
$if(display)$
        // The display mode that will be used to show slides
        display: $display$,
$endif$
$if(mathjax)$
        math: {
          mathjax: '$mathjaxurl$',
          config: 'TeX-AMS_HTML-full',
          tex2jax: {
            inlineMath: [['\\(','\\)']],
            displayMath: [['\\[','\\]']],
            balanceBraces: true,
            processEscapes: false,
            processRefs: true,
            processEnvironments: true,
            preview: 'TeX',
            skipTags: ['script','noscript','style','textarea','pre','code'],
            ignoreClass: 'tex2jax_ignore',
            processClass: 'tex2jax_process'
          },
        },
$endif$

        // reveal.js plugins
        plugins: [
$if(mathjax)$
          RevealMath,
$endif$
          RevealNotes,
          RevealSearch,
          RevealZoom
        ]
      });
    </script>
  $for(include-after)$
  $include-after$
  $endfor$
  </body>
</html>
