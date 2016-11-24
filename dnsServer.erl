% M-x erlang-shell
% > cd('home/raul/my-lab/erlang/dev-erlang/').
% > c(dnsServer).
% > dnsServer:run().

-module(dnsServer).
-export([run/0]).

-define(BYTE,8).
-define(DNS_ID_LEN(),(2*?BYTE)).
-define(DNS_FLAGS_LEN(),(2*?BYTE)).
-define(DNS_NUM_QUESTIONS_LEN(),(2*?BYTE)).
-define(DNS_QUESTIONS_LEN(),(1*?BYTE)).
-define(DNS_NUM_ANSWERS_LEN(),(2*?BYTE)).
-define(DNS_NUM_AUTH_LEN(),(2*?BYTE)).
-define(DNS_NUM_ADD_LEN(),(2*?BYTE)).
-define(DNS_HEADER_LEN(),(?DNS_ID_LEN()+?DNS_FLAGS_LEN()+?DNS_NUM_QUESTIONS_LEN()+?DNS_NUM_ANSWERS_LEN()+?DNS_NUM_AUTH_LEN()+?DNS_NUM_ADD_LEN())).

% hardcoded trivial values for this exercise
%
-define(DNS_FLAGS,16#8180).
-define(DNS_NUM_AUTH,16#0000).
-define(DNS_ANSWER_TYPE,16#0001). %  A (host address)
-define(DNS_ANSWER_CLASS,16#0001). % IN
-define(DNS_ANSWER_TTL,16#0000000A). % 10 seconds
-define(DNS_ANSWER_DATA_LENGTH,16#0004). % IPv4 ip address

dnsFilterCrLf(Data) ->
    re:replace(Data, "[\\n\\r]", "", [{return,list}]).

dnsConvertFileToList(S) ->
    case io:get_line(S, '') of
        eof ->
            [];
                Line -> [dnsFilterCrLf(Line) | dnsConvertFileToList(S)]
    end.

dnsConvertListToMap([]) ->    #{};
dnsConvertListToMap([Host|Hosts]) ->
    [Fqdn|Ips] = string:tokens(Host," "),
    IpsToBinaries = lists:map(fun erlang:list_to_binary/1,Ips),
    maps:put(Fqdn,IpsToBinaries,
             dnsConvertListToMap(Hosts)).

dnsGetHostsByFqdnChain([], Arg) ->
    Arg;
dnsGetHostsByFqdnChain([Fun | Funs], Arg) ->
    dnsGetHostsByFqdnChain(Funs, Fun(Arg)).

dnsGetHostsByFqdn(File) ->
    {ok, S} = file:open(File,read),

    %% Function composition to build the map of hosts from the
    %% configuration file
    Chain = [fun dnsConvertFileToList/1,fun dnsConvertListToMap/1],
    dnsGetHostsByFqdnChain(Chain,S).

dnsServer(Port,HostsByFqdn) ->
    {ok, Socket} = gen_udp:open(Port, [binary, {active,true}]),
    io:format("**DNS** server opened socket:~p~n",[Socket]),
    dnsReceive(Socket,HostsByFqdn).

dnsSend(Socket,HostsByFqdn,Host,Port,SrcPacket) ->

    % The request
    %
    <<Dns_id:?DNS_ID_LEN(),
      Dns_flags:?DNS_FLAGS_LEN(),
      Dns_num_questions:?DNS_NUM_QUESTIONS_LEN(),
      Dns_num_answers:?DNS_NUM_ANSWERS_LEN(),
      Dns_num_auth:?DNS_NUM_AUTH_LEN(),
      Dns_num_add:?DNS_NUM_ADD_LEN(),
      Dns_question/binary>> = SrcPacket,

    <<Dns_question_len:?DNS_QUESTIONS_LEN(),_/binary>> = Dns_question,
    Dns_question_len_bytes = Dns_question_len*?BYTE,
    <<_:?DNS_QUESTIONS_LEN(),
      Dns_question_name:Dns_question_len_bytes,_/binary>> = Dns_question,

    Name = binary_to_list(<<Dns_question_name:Dns_question_len_bytes>>),

    io:format("**DNS** queried: ~p~n",[Name]),
    
    % The response
    %
    DstPacket = <<Dns_id:?DNS_ID_LEN(),
                  ?DNS_FLAGS:?DNS_FLAGS_LEN(),
                  1:16,
                  ?DNS_NUM_AUTH:?DNS_NUM_QUESTIONS_LEN(),
                  ?DNS_NUM_AUTH:?DNS_NUM_AUTH_LEN(),
                  Dns_num_add:?DNS_NUM_ADD_LEN(),
                  <<"192.168.2.1">>/binary>>,

    gen_udp:send(Socket, Host, Port, DstPacket).

dnsReceive(Socket,HostsByFqdn) ->
    receive
        {udp, Socket, Host, Port, SrcPacket} = SrcData ->
            io:format("**DNS** server received:~p~n",[SrcData]),

            dnsSend(Socket,HostsByFqdn,Host,Port,SrcPacket),
            dnsReceive(Socket,HostsByFqdn)
            %% N = binary_to_term(SrcPacket),
            %% Fac = fac(N),
            %% gen_udp:send(Socket, Host, Port, term_to_binary(Fac)),
    end.

run() ->
    % Configuration data
    File = filename:join(filename:dirname(code:which(?MODULE)),"dns.hosts.txt"),
    Port = 3535,% Port > 1024 for non root testing

    io:format("**DNS** server received:~p~n",[code:which(?MODULE)]),
    HostsByFqdn = dnsGetHostsByFqdn(File),
    dnsServer(Port,HostsByFqdn).

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
