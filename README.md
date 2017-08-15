This repository was extracted from a larger internal project at
[Esper](https://esper.com).
We released it in the hope that it might be useful to other
OCaml developers.

It won't build as is but most of the code was used in production.

Description
-----------

Internal aggregator of data to be sent to Amazon Cloudwatch every minute, 
which is Cloudwatch's finest granularity.

Gator is a server that receives data of the form EVENT_NAME or (EVENT_NAME, 
NUMERIC_VALUE) over UDP. Every minute, for each event that was reported, gator 
sends stats to Cloudwatch using the `aws` command line, such as event 
count/frequency, min/med/max/mean value, etc.

This repository provides OCaml code for server and client. The use of UDP 
means the client doesn't need to open a connection and there won't be an error 
if the UDP packet gets lost or duplicated, which typically doesn't happen 
within EC2.
