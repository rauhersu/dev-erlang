% M-x erlang-shell
% > cd('home/raul/my-lab/erlang/dev-erlang/').
% > c(dnsServer).
% > dnsServer:run().

-module(dnsServer).
-export([run/0]).

-define(BYTE,
        8).
-define(NULL_TERMINATION_LEN,
        ?BYTE).

% DNS Query A Request
%
-define(DNS_ID_LEN(),
        (2*?BYTE)).
-define(DNS_FLAGS_LEN(),
        (2*?BYTE)).
-define(DNS_NUM_QUESTIONS_LEN(),
        (2*?BYTE)).
-define(DNS_QUESTIONS_LEN(),
        (1*?BYTE)).
-define(DNS_QTYPE_LEN(),
        (2*?BYTE)).
-define(DNS_QCLASS_LEN(),
        (2*?BYTE)).
-define(DNS_NUM_ANSWERS_LEN(),
        (2*?BYTE)).
-define(DNS_NUM_AUTH_LEN(),
        (2*?BYTE)).
-define(DNS_NUM_ADD_LEN(),
        (2*?BYTE)).
-define(DNS_HEADER_LEN(),
        (?DNS_ID_LEN()+
         ?DNS_FLAGS_LEN()+
         ?DNS_NUM_QUESTIONS_LEN()+
         ?DNS_NUM_ANSWERS_LEN()+
         ?DNS_NUM_AUTH_LEN()+
         ?DNS_NUM_ADD_LEN())).

% DNS Query A Response
%
-define(DNS_ANSWER_POINTER,
        16#C0).
-define(DNS_ANSWER_OFFSET(),
        (?DNS_HEADER_LEN() div ?BYTE)).
-define(DNS_ANSWER_TYPE_LEN(),
        (2*?BYTE)).
-define(DNS_ANSWER_CLASS_LEN(),
        (2*?BYTE)).
-define(DNS_ANSWER_TTL_LEN(),
        (4*?BYTE)).
-define(DNS_ANSWER_DATA_LENGTH_LEN(),
        (2*?BYTE)).
-define(DNS_ANSWER_ADDR_LEN(),
        (4*?BYTE)).

% hardcoded values for this exercise
%
-define(DNS_FLAGS,
        16#8180).
-define(DNS_NUM_AUTH,
        16#0000).
-define(DNS_NUM_ADD,
        16#0000).
-define(DNS_ANSWER_TYPE, % A (host address)
        16#0001).
-define(DNS_ANSWER_CLASS, % IN
        16#0001).
-define(DNS_ANSWER_TTL, % 10 seconds
        16#0000000A).
-define(DNS_ANSWER_DATA_LENGTH, % % 4 bytes (IPv4 address)
        16#0004).

% Executes a composition of functions
%
chainExec([], Arg) ->
    Arg;
chainExec([Fun | Funs], Arg) ->
    chainExec(Funs, Fun(Arg)).

dnsFilterCrLf(Data) ->
    re:replace(Data, "[\\n\\r]", "", [{return,list}]).

% TODO: TCO
% 
dnsConvertFileToList(S) ->
    case io:get_line(S, '') of
        eof -> [];
        Line -> [dnsFilterCrLf(Line) | dnsConvertFileToList(S)]
    end.
dnsParseAddresss(Ip) ->
    element(2,inet:parse_address(Ip)).


% TODO: TCO
% 
dnsConvertListToMap([]) ->
    #{};
dnsConvertListToMap([Host|Hosts]) ->
    [Name|Ips] = string:tokens(Host," "),

    % Composes Ips in a binary (<<...>>) form.
    % Is there a shorter way rather than 3 funs?!
    Chain = [fun dnsParseAddresss/1,
             fun tuple_to_list/1,
             fun list_to_binary/1],
    Fun = fun (X) -> chainExec(Chain,X) end,

    IpsToBinaries = lists:map(Fun,Ips),
    maps:put(Name,IpsToBinaries,dnsConvertListToMap(Hosts)).

dnsGetHostsByName(File) ->
    {ok, S} = file:open(File,read),

    % Composes a map of Host -> IPs
    %
    Chain = [fun dnsConvertFileToList/1,
             fun dnsConvertListToMap/1],
    chainExec(Chain,S).


% Host found
%
dnsSendAnswer({ok,HostIPs},
              Dns_id,Dns_host_name,Dns_host_name_len,Dns_rest_of_msg) ->

    io:format("**DNS** hostname found!~n"),

    % get the query A (host name + QTYPE + QCLASS))
    %
    Dns_queryA_len =
        ?DNS_QUESTIONS_LEN()+
        Dns_host_name_len+
        ?NULL_TERMINATION_LEN+
        ?DNS_QTYPE_LEN()+
        ?DNS_QCLASS_LEN(),

    <<Dns_queryA:Dns_queryA_len,
      _/binary>> = Dns_rest_of_msg,
    
    <<Dns_id:?DNS_ID_LEN(),
      ?DNS_FLAGS:?DNS_FLAGS_LEN(),
      1:?DNS_NUM_QUESTIONS_LEN(),
      1:?DNS_NUM_ANSWERS_LEN(),
      ?DNS_NUM_AUTH:?DNS_NUM_AUTH_LEN(),
      ?DNS_NUM_ADD:?DNS_NUM_ADD_LEN(),
      Dns_queryA:Dns_queryA_len,
      % Pointer to the hostname (it is present in the queryA)
      % See 4.1.4 "Message compression" on RFC1035
      ?DNS_ANSWER_POINTER:?BYTE,
      % Offset for that pointer (hostname = pointerFromStart+offset)
      ?DNS_ANSWER_OFFSET():?BYTE,
      ?DNS_ANSWER_TYPE:?DNS_ANSWER_TYPE_LEN(),
      ?DNS_ANSWER_CLASS:?DNS_ANSWER_CLASS_LEN(),
      ?DNS_ANSWER_TTL:?DNS_ANSWER_TTL_LEN(),
      ?DNS_ANSWER_DATA_LENGTH:?DNS_ANSWER_DATA_LENGTH_LEN(),
      <<192,168,2,1>>/binary,
      ?DNS_NUM_ADD:?DNS_NUM_ADD_LEN()>>;

% Host not found
%
dnsSendAnswer(error,
              Dns_id,Dns_host_name,Dns_host_name_len, Dns_rest_of_msg) ->

    io:format("**DNS** hostname NOT found!~n"),


    % get the query A (host name + QTYPE + QCLASS))
    %
    Dns_queryA_len =
        ?DNS_QUESTIONS_LEN()+
        Dns_host_name_len+
        ?NULL_TERMINATION_LEN+
        ?DNS_QTYPE_LEN()+
        ?DNS_QCLASS_LEN(),

    <<Dns_queryA:Dns_queryA_len,
      _/binary>> = Dns_rest_of_msg,

    <<Dns_id:?DNS_ID_LEN(),
      ?DNS_FLAGS:?DNS_FLAGS_LEN(),
      1:?DNS_NUM_QUESTIONS_LEN(),
      0:?DNS_NUM_ANSWERS_LEN(),   % TODO: a better value
      ?DNS_NUM_AUTH:?DNS_NUM_AUTH_LEN(),
      ?DNS_NUM_ADD:?DNS_NUM_ADD_LEN(),
      Dns_queryA:Dns_queryA_len>>.

dnsProcessQueryA(Socket,HostsByName,Host,Port,SrcPacket) ->

    % The request
    %
    <<Dns_id:?DNS_ID_LEN(),
      _Dns_flags_questions_answers_numauth_numadd:(?DNS_HEADER_LEN()-
                                                       ?DNS_ID_LEN()),
      Dns_rest_of_msg/binary>> = SrcPacket,

    % get the host name
    %
    <<Dns_host_name_len_bytes:?DNS_QUESTIONS_LEN(),
      _/binary>> = Dns_rest_of_msg,

    Dns_host_name_len = Dns_host_name_len_bytes*?BYTE,

    <<_len:?DNS_QUESTIONS_LEN(),
      Dns_host_name:Dns_host_name_len,_/binary>> = Dns_rest_of_msg,

    HostName = binary_to_list(<<Dns_host_name:Dns_host_name_len>>),
    io:format("**DNS** queried: ~p~n",[HostName]),

    HostInfo = maps:find(HostName,HostsByName),

    % The response
    %
    DstPacket = dnsSendAnswer(HostInfo,
                              Dns_id,
                              Dns_host_name,
                              Dns_host_name_len,
                              Dns_rest_of_msg),

    io:format("**DNS** DstPacket:~p~n",[DstPacket]),

    gen_udp:send(Socket, Host, Port, DstPacket).


% TODO: this is a sequential server: spawn a server per query
%
dnsReceive(Socket,HostsByName) ->
    receive
        {udp, Socket, Host, Port, SrcPacket} = SrcData ->
            io:format("**DNS** server received:~p~n",[SrcData]),

            dnsProcessQueryA(Socket,HostsByName,Host,Port,SrcPacket),
            dnsReceive(Socket,HostsByName)
    end.

dnsServer(Port,HostsByName) ->
    {ok, Socket} = gen_udp:open(Port, [binary, {active,true}]),
    io:format("**DNS** server opened socket:~p~n",[Socket]),
    dnsReceive(Socket,HostsByName).

run() ->
    % Code and configuration file in the same dir (MODULE trick)
    File = filename:join(filename:dirname(code:which(?MODULE)),"dns.hosts.txt"),

    % Port > 1024 for non root access (make sure not in use)
    Port = 3535,

    io:format("**DNS** server port:~p file:~p~n",[Port,File]),

    HostsByName = dnsGetHostsByName(File),

    % Start listening requests...
    io:format("**DNS** server configured with:~p~n",[HostsByName]),

    dnsServer(Port,HostsByName).
