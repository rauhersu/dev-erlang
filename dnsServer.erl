-module(dnsServer).
-export([run/0]).

dnsFilterCrLf(Data) ->
    re:replace(Data, "[\\n\\r]", "", [{return,list}]).

dnsConvertFileToList(S) ->
    case io:get_line(S, '') of
        eof -> [];
        Line -> [dnsFilterCrLf(Line) | dnsConvertFileToList(S)]
    end.

dnsConvertListToMap([]) ->
    #{};
dnsConvertListToMap([Host|Hosts]) ->
    [Fqdn|IPs] = string:tokens(Host," "),
    maps:put(Fqdn,IPs,dnsConvertListToMap(Hosts)).

dnsGetHostsByFqdnChain([], Arg) ->
    Arg;
dnsGetHostsByFqdnChain([Fun | Funs], Arg) ->
    dnsGetHostsByFqdnChain(Funs, Fun(Arg)).

dnsGetHostsByFqdn(File) ->
    {ok, S} = file:open(File,read),

    %% A chain of responsibility: from a File to a Map 
    Chain = [fun dnsConvertFileToList/1,fun dnsConvertListToMap/1],
    dnsGetHostsByFqdnChain(Chain,S).

run() ->    
    File = "/home/raul/my-lab/erlang/dns.hosts.txt",
    HostsByFqdn = dnsGetHostsByFqdn(File).

    %% {ok, S} = file:open(File,read),
    %% Hosts = dnsConvertFileToList(S,fun dnsFilterCrLf/1),
    %% dnsConvertListToMap(Hosts).

% Chain of responsibility in Erlang:

% http://www.erlangpatterns.org/chain.html

% 1. Quieres llegar aquí:

% 1.Necesitarás tokenizar cada elemento de la lista actual, 'tokens()'
%   http://erlang.org/doc/man/string.html#words-1
% 2.Necesitarás formar una lista de tuplas para aplicar '3'
% 3.Aplica la conversión a map desde una lista de tuplas, 'fromlist()'
%   http://erlang.org/doc/man/maps.html#from_list-1

% TODO: 
% 1. Mira que la funcion que has hecho no es tail recursive, conviertela
% 2. Cuando sea un tipo como singleton, usa 'The...' (ej, para el mapping de ficheros)
% 3. El fichero de texto como parametro del argv

