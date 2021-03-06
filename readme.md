### Censorship environments: 

1. websites are blocked >> CONNECT == SUCCESS  
2. website and tor public relays are blocked >> CHOOSE ANY BRIDGE == SUCCESS  
3. website, tor public relays, and tor hard coded bridges are blocked >> MEEK OR OWN BRIDGE ADDRESS == SUCCESS   

### Participant tasks: 
* Go to cnn.com (non-blocked website) and write the headline. 
* Go to wikipedia.org (blocked website) and write down what the topic of the day is.

### Hypotheses: 

1. people don't know what a bridge or proxy is. > ask participants to define these terms in the survey.  
2. people don't know what the difference between a bridge and a proxy is > ask if people can distinguish them. 
3. people do not know when bridges or proxies are necessary > ask them this in the survey.  
4. proxies cause confusion when configuring bridges. screen recording: see if people click on proxies (none of the enviornments require a proxy)
5. people will favor familiar-sounding transports (i.e. meek-google versus meek-azure or scramblesuit) > see screen recordings, ask in survey for which transports were picked in which order and why they chose the final transport.  
6. (some dialogue is redundant. see opening window: "before you connect..." and bridge2 window: "you may use...") > we can try asking if people find these things useful, if they would prefer the interface without it, or if they read it.  

### Tor Survey Questions: 
http://www.surveygizmo.com/s3/2085559/Tor-Usability-Survey/SG_TEST_RUN

### Graphvis commands (for generating interface flow diagram):  
* generate png for digraph: dot -Tpng torconfig.dot > torconfig.png
* generate pdf for digraph: dot -Tpdf torconfig.dot > torconfig.pdf
Tor Circumvention UX Experiment

### Scope: 
- Tor’s configuration dialogue only, with respect to censorship circumvention
! Not in scope: proxies, user trust models, rest of Tor’s UX, anonymity

### Data collected: 

* configuration control flow: via video capture
* participants information/feedback: via survey

### Analyses: 
* show a markov chain version of the configuration tree, before and after changes
* qualitative analysis on survey answers 

### Patches to Tor Browser:

https://github.com/nmalkin/tor-launcher
(See branches [instrumented](https://github.com/nmalkin/tor-launcher/tree/instrumented) and [uxtest](https://github.com/nmalkin/tor-launcher/tree/uxtest).)
