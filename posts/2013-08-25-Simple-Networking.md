---
title: Simple Networking
description: communicating over UDP
tags: udp, networking, haskell, C++
---

Last Friday a [coworker] was so kind to organize a fun session at work where we had to write bots that participated in a [game](http://en.wikipedia.org/wiki/Mia_%28game%29). The basic idea is to program a bot that plays against other bots, all communicating via UDP with a server that supervises the game.  
Even though many of our developers are rather avid C++ users, this problem involved two reoccurring tasks: UDP communication and string handling, both topics that some would claim are not the particular strong points of C++ (up to the point that it is getting [ridiculous])[^1]. So most of us settled on [Ruby] or [Python], both of which shine especially with string processing.  
Haskell supposedly also makes it easy to work with datagram sockets but it always takes me some time to get it right so this time I wanted to take the time to write down a small UDP sample for future reference. (there is an abundance of TCP examples out there, but less so for UDP)

Here is a pretty minimal UDP client application in haskell that just connects a datagram socket and sends some data.  
I retrieve the address-information for the server I want to talk to (`getAddrInfo`), get a socket (`socket`) and associate it with the server ip and port (`connect`). Now I have a socket that can be used to send datagram packets.  
For completeness I wrap the code with `bracket` just to be sure clean up is done properly in every case. Don't mind the `withSocketsDo`, this is only necessary on windows for some initialization stuff but usually is included to make the code platform agnostic.

~~~ {.haskell}
module Main where

import Network.Socket
import Control.Exception

port = "3000"

main = withSocketsDo $ bracket getSocket sClose talk
        where getSocket = do
                (serveraddr:_) <- getAddrInfo Nothing (Just "127.0.0.1") (Just port)
                s <- socket (addrFamily serveraddr) Datagram defaultProtocol
                connect s (addrAddress serveraddr) >> return s
              talk s = do
                send s "Hello, world!"
                recv s 1024 >>= \msg -> putStrLn $ "Received " ++ msg
~~~

Even though in our scenario writing a server was not required, it proved valuable for test purposes and I include it for reference.  
Here, we build a socket with the address-info and bind it to our own ip on our port (`getaddrinfo`,`socket`,`bindSocket`). Having bound the socket, we can receive from it and send s.th. back to the client that sent us a message.

~~~ {.haskell}
module Main where

import Control.Monad (unless)
import Network.Socket
import Control.Exception

port = "3000"

main = withSocketsDo $ bracket connectMe sClose handler
          where
            connectMe = do
              (serveraddr:_) <- getAddrInfo
                                  (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                                  Nothing (Just port)
              sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
              bindSocket sock (addrAddress serveraddr) >> return sock

handler :: Socket -> IO ()
handler conn = do
    (msg,n,d) <- recvFrom conn 1024
    putStrLn $ "< " ++ msg
    unless (null msg) $ sendTo conn msg d >> handler conn
~~~

<pre class="terminal">
$ runghc client
Received Hello, world!
</pre>

For our bot contest I paired with a ruby guy and a simple UPD client is very easy, even though it does not include any options to setup the address-info or handle graceful shutdown in case of exceptions:

~~~ {.ruby}
require 'socket'

port = 3000

sock = UDPSocket.new
data = 'Hello Server'
sock.send(data, 0, '127.0.0.1', port)
resp = sock.recv(1024)
p resp
sock.close
~~~

<pre class="terminal">
$ ruby client.rb
"Hello Server"
</pre>

Pretty interesting to see that even developers with a strong C/C++ background had problems getting started. But then again it's definitely more work to get a working client using the posix api. For the fun, here is the code:

~~~ {.cpp}
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <iostream>

using namespace std;

#define SERVERPORT "3000"
#define SERVERIP "127.0.0.1"
enum { MAX_BUFFER_SIZE = 1024 };

int main(int argc, char* argv[])
{
    struct addrinfo hints, *servinfo, *p;
    const char* msg = "Hello World";

    memset(&hints, 0, sizeof hints);
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_DGRAM;

    getaddrinfo(SERVERIP, SERVERPORT, &hints, &servinfo);

    int sockfd;
    for (p = servinfo; p != NULL; p = p->ai_next)
    {
        sockfd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
        if (sockfd == -1)
        {
            continue;
        }
        break;
    }

    if (p == NULL)
    {
        cerr << "client: could not bind socket" << endl;
        return 2;
    }
    int sent = sendto(sockfd, msg, strlen(msg), 0, p->ai_addr, p->ai_addrlen);
    cout << "sent " << sent << " bytes to " << SERVERIP << endl;
    char buffer[MAX_BUFFER_SIZE];
    int bytesReceived = recv( sockfd, buffer, MAX_BUFFER_SIZE-1, 0 );
    buffer[bytesReceived]= '\0';
    cout << "received back " << bytesReceived << " bytes: " << buffer << endl;
    freeaddrinfo(servinfo);

    close(sockfd);
    return 0;
}
~~~

<pre class="terminal">
$ clang++ -Wall client.cpp -o c
$ ./c
sent 11 bytes to 127.0.0.1
received back 11 bytes: Hello World
</pre>

Besides all the technical details, it is a great way to spend a Friday afternoon and more companies should consider allowing for such fun events!

[^1]: Yes, I know about [asio] and [boost string algorithms]

[Ruby]:http://ruby-doc.org/core-2.0/String.html
[Python]:http://docs.python.org/2/library/string.html
[coworker]:http://sebastianbenz.de/
[ridiculous]:http://stackoverflow.com/questions/236129/splitting-a-string-in-c
[asio]:http://www.boost.org/doc/libs/1_54_0/doc/html/boost_asio.html
[boost string algorithms]:http://www.boost.org/doc/libs/1_54_0/doc/html/string_algo.html

