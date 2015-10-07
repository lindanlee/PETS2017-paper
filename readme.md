TODO: 
-run experiment to evaluate the current configuration interface. 
-run an experiment to evaluate an improved interface.
-run an experiment to evaluate an alternate interface. 

Censorship environments: 
1. websites are blocked >> CONNECT == SUCCESS
2. website and tor public relays are blocked >> CHOOSE ANY BRIDGE == SUCCESS
3. website, tor public relays, and tor hard coded bridges are blocked >> MEEK OR OWN BRIDGE ADDRESS == SUCCESS 

Participant tasks: 
• Google search for the population of Zimbabwe.
• On YouTube, find a video playing Bach’s “Ode to Joy.”
• Find the Amazon best-sellers in “Movies & TV.”
• On Yahoo, find the exchange rate of Dollars to Euros.
• Find the Wikipedia “History” portal’s featured article.
• On Twitter, find the currently trending topics.
• On Bing Maps, find directions from Times Square to
Carnegie Hall.

Tor Survey Questions: 
http://www.surveygizmo.com/s3/2085559/Tor-Usability-Survey/SG_TEST_RUN

Graphvis commands (for generating interface flow diagram):  
* generate png for digraph: dot -Tpng torconfig.dot > torconfig.png
* generate pdf for digraph: dot -Tpdf torconfig.dot > torconfig.pdf
Tor Circumvention UX Experiment

Scope: 
- Tor’s configuration dialogue only, with respect to censorship circumvention
! Not in scope: proxies, user trust models, rest of Tor’s UX, anonymity

Data collected: 
-configuration control flow: via video capture
-participants information/feedback: via survey

Analyses: 
* show a markov chain version of the configuration tree, before and after changes
* qualitative analysis on survey answers 

