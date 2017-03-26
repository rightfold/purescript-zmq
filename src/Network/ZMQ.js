'use strict';

var Control_Monad_Aff = require('../Control.Monad.Aff');
var zeromq = require('zeromq');

exports.defaultContext = function(onSuccess, onError) {
  onSuccess(zeromq);
  return Control_Monad_Aff.nonCanceler;
};

exports.newSocket = function(socketType) {
  return function(context) {
    return function(onSuccess, onError) {
      var socket;
      try {
        socket = context.socket(socketType.socketTypeName(null));
      } catch (e) {
        onError(e);
        return Control_Monad_Aff.nonCanceler;
      }
      onSuccess(socket);
      return Control_Monad_Aff.nonCanceler;
    };
  };
};

exports.closeSocket = function(socket) {
  return function(onSuccess, onError) {
    try {
      socket.close();
    } catch (e) {
      onError(e);
      return Control_Monad_Aff.nonCanceler;
    }
    onSuccess(null);
    return Control_Monad_Aff.nonCanceler;
  };
};

exports.bindSocket = function(socket) {
  return function(address) {
    return function(onSuccess, onError) {
      socket.bind(address, function(err) {
        if (err !== undefined) {
          onError(err);
          return;
        }
        onSuccess(null);
      });
      return Control_Monad_Aff.nonCanceler;
    };
  };
};

exports.connectSocket = function(socket) {
  return function(address) {
    return function(onSuccess, onError) {
      try {
        socket.connect(address);
      } catch (e) {
        onError(e);
        return Control_Monad_Aff.nonCanceler;
      }
      onSuccess(null);
      return Control_Monad_Aff.nonCanceler;
    };
  };
};
