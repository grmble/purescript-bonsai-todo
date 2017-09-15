"use strict";

exports.getItemFn3 = function (key, nothing, just) {
  var value = window.localStorage.getItem(key);
  if (value) {
    return just(value);
  } else {
    return nothing;
  }
};

exports.setItem = function (key) {
  return function (value) {
    return function () {
      return window.localStorage.setItem(key, value);
    };
  };
};

exports.removeItem = function (key) {
  return function () {
    return window.localStorage.removeItem(key);
  };
};
