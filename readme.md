dot -Tpng torconfig.dot > torconfig.png
dot -Tpdf torconfig.dot > torconfig.pdf

VISIONS: 
 * have UI paths for Tor vs other browsers--is is usable? how is it different? the number of choices, 
 the breadth of the paths, the depth of the paths, etc. 
 * compare browser states for Tor vs other browsers--how is it? number of states, types of states, etc.
 * visualize how people fail: set up a video of the UI config path in Tor, and how people traverse it 
 over time, lighting up the paths over time and indicating end result of fail or success
 * visualize people in environments in aggregate: grid of all the animations of the single UI config
 trees in a grid, all people on same environment in a grid, clustered by behavior, and playing while 
 a timer displays the time. 
 * fix paths: consolidate good paths and bad paths, reduce number of paths, prevent mistakes.  


META-TODO: 
 * ~visualizations of UI config paths in Tor~ (for user experiments!)
 * determine which UI config paths work/don't work for all three censorship environments
 * draft a survey: get at what people are confused about and why
 * think of UI tweak to test: reducing depth and breadth of UI paths
 * hash out alternative: auto-config; is it ethical if less mistakes but no control? 

 * determine which UI config paths work/don't work for countries (China, Iran..) 
 * visualizations of UI paths for all of Tor
 * visualizations of UI paths for browsers (Chrome, Firefox, Safari, IE) 
 * determine how many browser states based on choices; analyze for paths, reversibility, etc.

TODO:
 * Expand error states from progress0 (e.g., you need PTs or you need bridges
   and haven't configured them).
