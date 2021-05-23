"use strict";

exports.unsafeParseInt = function unsafeParseInt(input) {
    return parseInt(input,10);
};

exports.delay_ = function delay(ms) {
    return function(f) {
        return function() {
            var tid = setTimeout(f, ms);
            return function() {clearTimeout(tid);};
        };
    };
};
