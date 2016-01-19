#!/usr/bin/node

var n = parseInt(process.argv[2]);
var fn = process.argv[3];
var fs = require("fs");

fs.readFile(fn, function(err, d){
  var lines = d.toString().split("\n");
  lines.forEach(function(l){
    var f = l.split("\t");
    if(parseInt(f[1]) > n){
      console.log(l);
    }
  });
});
