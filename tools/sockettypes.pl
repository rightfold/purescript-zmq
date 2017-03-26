#!/usr/bin/env perl
use strict;
use warnings;

my @types;

while (<DATA>) {
    s/^, |:.*$//g;
    chomp;
    push @types, $_;
}

for (@types) {
    my $p = ' ' x (6 - length);
    print "foreign import data " . uc . $p . " :: SocketType\n";
}

for (@types) {
    my $p = ' ' x (6 - length);
    print "\n";
    print "instance socketType" . uc . $p . " :: SocketType " . uc . $p . " where socketTypeName _ = \"" . lc . "\"";
}

__DATA__
, pub: zmq.ZMQ_PUB
, xpub: zmq.ZMQ_XPUB
, sub: zmq.ZMQ_SUB
, xsub: zmq.ZMQ_XSUB
, req: zmq.ZMQ_REQ
, xreq: zmq.ZMQ_XREQ
, rep: zmq.ZMQ_REP
, xrep: zmq.ZMQ_XREP
, push: zmq.ZMQ_PUSH
, pull: zmq.ZMQ_PULL
, dealer: zmq.ZMQ_DEALER
, router: zmq.ZMQ_ROUTER
, pair: zmq.ZMQ_PAIR
, stream: zmq.ZMQ_STREAM
