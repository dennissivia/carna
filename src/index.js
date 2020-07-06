"use strict"

require("ace-css/css/ace.css")
require("font-awesome/css/font-awesome.css")

// Require so it gets copied to dist
require("./styles.css")

var Elm = require("./Main.elm")
var lang = navigator.language || navigator.userLanguage || "en-EN"
var initialBodyIndex = localStorage.getItem("lastBodyIndexData")
console.log("initial bodyIndex from localstore: ", initialBodyIndex)

if (lang.indexOf("de") !== -1) {
  document.documentElement.lang = "de"
} else {
  document.documentElement.lang = "en"
}

var app = Elm.Main.fullscreen({
  userLanguage: lang,
  initialBodyIndex: initialBodyIndex,
})

app.ports.trackHashPage.subscribe(function (path) {
  if (typeof ga === "undefined") {
    return
  }
  ga("set", "page", path)
  ga("send", "pageview")
})

app.ports.trackBodyIndexSubmit.subscribe(function () {
  if (typeof ga === "undefined") {
    return
  }
  ga("send", {
    hitType: "event",
    eventCategory: "BodyIndex",
    eventAction: "FormSubmit",
  })
})

app.ports.trackBodyFatSubmit.subscribe(function () {
  if (typeof ga === "undefined") {
    return
  }
  ga("send", {
    hitType: "event",
    eventCategory: "BodyFat",
    eventAction: "FormSubmit",
  })
})

app.ports.saveBodyIndex.subscribe(function (record) {
  console.log("Saving record to localstore: ", record)
  localStorage.setItem("lastBodyIndexData", JSON.stringify(record))
})

// Not needed for now, because we only load on boot start via flags
//app.ports.doloadBodyIndex.subscribe(function() {
//  console.log("loading record: ", record);
//  app.ports.loadBodyIndex.send(Json.parse(localStorage.getItem("lastBodyIndexData")));
//});
