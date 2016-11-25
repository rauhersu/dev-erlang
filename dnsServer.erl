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
-define(DNS_FLAGS,
        16#8180).
-define(DNS_NUM_AUTH,
        16#0000).
-define(DNS_ANSWER_TYPE, % A (host address)
        16#0001).
-define(DNS_ANSWER_CLASS, % IN
        16#0001).
-define(DNS_ANSWER_TTL, % 10 seconds
        16#0000000A).
-define(DNS_ANSWER_DATA_LENGTH, % % 4 bytes (IPv4 address)
        16#0004).

%% Function composition (chained functions)
%%
chainExec([], Arg) ->
    Arg;
chainExec([Fun | Funs], Arg) ->
    chainExec(Funs, Fun(Arg)).

dnsFilterCrLf(Data) ->
    re:replace(Data, "[\\n\\r]", "", [{return,list}]).

dnsConvertFileToList(S) ->
    case io:get_line(S, '') of
        eof -> [];
        Line -> [dnsFilterCrLf(Line) | dnsConvertFileToList(S)]
    end.
dnsParseAddresss(Ip) ->
    element(2,inet:parse_address(Ip)).

dnsConvertListToMap([]) ->
    #{};
dnsConvertListToMap([Host|Hosts]) ->
    [Fqdn|Ips] = string:tokens(Host," "),

    %% From IPs in a string form to a binary form.
    %% Is there an shorter way??
    Chain = [fun dnsParseAddresss/1,
             fun tuple_to_list/1,
             fun list_to_binary/1],
    Fun = fun (X) -> chainExec(Chain,X) end,

    IpsToBinaries = lists:map(Fun,Ips),
    maps:put(Fqdn,IpsToBinaries,dnsConvertListToMap(Hosts)).

dnsGetHostsByFqdn(File) ->
    {ok, S} = file:open(File,read),

    %% From IPs in the configuration file to map of (Host,IPs)
    %%
    Chain = [fun dnsConvertFileToList/1,
             fun dnsConvertListToMap/1],
    chainExec(Chain,S).

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
    <<_len:?DNS_QUESTIONS_LEN(),
      Dns_question_name:Dns_question_len_bytes,_/binary>> = Dns_question,

    Name = binary_to_list(<<Dns_question_name:Dns_question_len_bytes>>),

    io:format("**DNS** queried: ~p~n",[Name]),

    % The response
    %
%% DstPacket = <<Dns_id:?DNS_ID_LEN(),
%%                   ?DNS_FLAGS:?DNS_FLAGS_LEN(),
%%                   1:16,
%%                   ?DNS_NUM_AUTH:?DNS_NUM_QUESTIONS_LEN(),
%%                   ?DNS_NUM_AUTH:?DNS_NUM_AUTH_LEN(),
%%                   Dns_num_add:?DNS_NUM_ADD_LEN(),
%%               <<"192.168.2.1">>/binary>>,


%% Dns_flags

    DstPacket = <<Dns_id:?DNS_ID_LEN(),
                  ?DNS_FLAGS:?DNS_FLAGS_LEN(),
                  1:16, % 1 Question
                  1:16, % 1 Answer
                  1:16,
                  1:16,
                  Dns_question_name:Dns_question_len_bytes,
                  <<192,168,2,1>>/binary,
                  Dns_num_auth:?DNS_NUM_AUTH_LEN(),
                  Dns_num_add:?DNS_NUM_ADD_LEN()>>,

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
    % Code and configuration file in the same dir (MODULE trick)
    File = filename:join(filename:dirname(code:which(?MODULE)),"dns.hosts.txt"),

    % Port > 1024 for non root testing. Make sure not in use
    Port = 3535,

    HostsByFqdn = dnsGetHostsByFqdn(File),

    % Start listening requests...
    io:format("**DNS** server configured with:~p~n",[HostsByFqdn]),

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
% 3. El fichero de texto como parametro del argv ->
