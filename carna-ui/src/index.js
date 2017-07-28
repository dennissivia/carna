'use strict';

require('ace-css/css/ace.css');
require('font-awesome/css/font-awesome.css');

// Require so it gets copied to dist
require('./styles.css');

var Elm = require('./Main.elm');
var lang = navigator.language || navigator.userLanguage || 'en-EN';

if(lang.indexOf("de") !== -1){
    document.documentElement.lang = "de";
}else{
    document.documentElement.lang = "en";
}

var app = Elm.Main.fullscreen({userLanguage: lang});

app.ports.trackHashPage.subscribe(function(path) {
    ga('set', 'page', path);
    ga('send', 'pageview');
});

app.ports.trackBodyIndexSubmit.subscribe(function(){
    ga('send', {
        hitType: 'event',
        eventCategory: 'BodyIndex',
        eventAction: 'FormSubmit'
    });

});

app.ports.trackBodyFatSubmit.subscribe(function(){
    ga('send', {
        hitType: 'event',
        eventCategory: 'BodyFat',
        eventAction: 'FormSubmit'
    });

});
