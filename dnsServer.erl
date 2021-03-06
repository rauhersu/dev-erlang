% M-x erlang-shell
% > cd('home/raul/my-lab/erlang/dev-erlang/').
% > c(dnsServer).
% > dnsServer:run().

-module(dnsServer).
-export([run/1]).

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
-define(DNS_QUESTION_ONE, % only one question is allowed
        1).

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
-define(DNS_ANSWER_ZERO,
        0).

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

-include_lib("stdlib/include/qlc.hrl").
-record(dns_queryA_response_TABLE, {hostname, responses}).

dns_db_init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(dns_queryA_response_TABLE,
                        [{attributes, record_info(fields,
                                                  dns_queryA_response_TABLE)}]),
    mnesia:stop().

dns_db_start() ->
    mnesia:start(),
    mnesia:wait_for_tables([dns_queryA_response_TABLE], 20000).

dns_format_queryA_response_TABLE(Rows)->
    Fun = fun (Row) ->
                  io:format("~-15s : ~-50s ~n~-15s : ~15p ~n~n",
                            ["HOSTNAME",Row#dns_queryA_response_TABLE.hostname,
                             "RESPONSES",Row#dns_queryA_response_TABLE.responses])
          end,
    lists:map(Fun, Rows).

dns_db_show_queryA_response(Format) ->
    _Rows = do(qlc:q([X || X <- mnesia:table(dns_queryA_response_TABLE)])),
    Format(_Rows).

dns_db_add_queryA_response(Hostname) ->
    Transaction =
        fun() ->
                [Host] = mnesia:read({dns_queryA_response_TABLE,Hostname}),
                Host_responses = Host#dns_queryA_response_TABLE.responses,
                Host_responses_now = Host#dns_queryA_response_TABLE{responses =
                                                              Host_responses+1},
                mnesia:write(Host_responses_now)
        end,
    mnesia:transaction(Transaction).

dns_db_provision(Hosts_by_name) ->
    Hostnames = maps:keys(Hosts_by_name),
    Populate = fun(Hostname) ->
                       {dns_queryA_response_TABLE,Hostname,_Response=0} end,
    Rows = lists:map(Populate, Hostnames),

    Transaction = fun() -> lists:foreach(fun mnesia:write/1, Rows) end,
    mnesia:transaction(Transaction).

do(Q) ->
    Transaction = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(Transaction),
    Val.

% Executes a composition of functions
%
chain_exec([], Arg) ->
    Arg;
chain_exec([Fun | Funs], Arg) ->
    chain_exec(Funs, Fun(Arg)).

dns_filter_cr_lf(Data) ->
    re:replace(Data, "[\\r\\n]", "", [{return,list}]).

% TODO: TCO
%
dns_convert_file_to_list(S) ->
    case io:get_line(S, '') of
        eof -> [];
        Line -> [dns_filter_cr_lf(Line) | dns_convert_file_to_list(S)]
    end.
dns_parse_address(Ip) ->
    element(2,inet:parse_address(Ip)).

% TODO: TCO
%
dns_convert_list_to_map([]) ->
    #{};
dns_convert_list_to_map([Host|Hosts]) ->
    [Name|Ips] = string:tokens(Host," "),

    % Composes Ips in a binary (<<...>>) form.
    % Is there a shorter way rather than 3 funs?!
    Chain = [fun dns_parse_address/1,
             fun tuple_to_list/1,
             fun list_to_binary/1],
    Fun = fun (X) -> chain_exec(Chain,X) end,

    Ips_to_binaries = lists:map(Fun,Ips),
    maps:put(Name,Ips_to_binaries,dns_convert_list_to_map(Hosts)).

dns_get_hosts_by_name(File) ->
    {ok, S} = file:open(File,read),

    % Composes a map of Host -> IPs
    %
    Chain = [fun dns_convert_file_to_list/1,
             fun dns_convert_list_to_map/1],
    chain_exec(Chain,S).

dns_encode_queryA_header_and_question(Dns_id,
                                      Dns_num_answers,
                                      Dns_queryA,
                                      Dns_queryA_len) ->

    <<Dns_id:?DNS_ID_LEN(),
      ?DNS_FLAGS:?DNS_FLAGS_LEN(),
      ?DNS_QUESTION_ONE:?DNS_NUM_QUESTIONS_LEN(),
      Dns_num_answers:?DNS_NUM_ANSWERS_LEN(),
      ?DNS_NUM_AUTH:?DNS_NUM_AUTH_LEN(),
      ?DNS_NUM_ADD:?DNS_NUM_ADD_LEN(),
      Dns_queryA:Dns_queryA_len>>.

dns_encode_queryA_answers(Host_ips) ->

    Dns_encode_one_queryA_answer = fun (Host_ip) ->
      % Pointer to the hostname (it is already present in the queryA segment)
      % See 4.1.4 "Message compression" on RFC1035
      <<?DNS_ANSWER_POINTER:?BYTE,
        % Offset for that pointer (hostname = pointerFromStart+offset)
        ?DNS_ANSWER_OFFSET():?BYTE,
        ?DNS_ANSWER_TYPE:?DNS_ANSWER_TYPE_LEN(),
        ?DNS_ANSWER_CLASS:?DNS_ANSWER_CLASS_LEN(),
        ?DNS_ANSWER_TTL:?DNS_ANSWER_TTL_LEN(),
        ?DNS_ANSWER_DATA_LENGTH:?DNS_ANSWER_DATA_LENGTH_LEN(),
        Host_ip/binary>> end,

    All_QueryA_answers =
        erlang:iolist_to_binary(lists:map(Dns_encode_one_queryA_answer,
                                                            Host_ips)),

    _All_QueryA_answers_plus_add = erlang:iolist_to_binary([All_QueryA_answers,
                                         <<?DNS_NUM_ADD:?DNS_NUM_ADD_LEN()>>]).
% Host found
%
dns_encode_queryA_response({ok,Host_ips},
                            Hostname,Dns_id,Dns_queryA,Dns_queryA_len) ->

    io:format("**DNS** hostname:~p found!~n",[Hostname]),

    dns_db_add_queryA_response(Hostname),

    Num_answers = erlang:length(Host_ips),

    QueryA_header_and_question =
        dns_encode_queryA_header_and_question(Dns_id,
                                          Num_answers,
                                           Dns_queryA,
                                      Dns_queryA_len),

    QueryA_anwers = dns_encode_queryA_answers(Host_ips),
    _QueryA_response =
        erlang:iolist_to_binary([QueryA_header_and_question,QueryA_anwers]);

% Host not found
%
dns_encode_queryA_response(error,
                           Hostname,Dns_id,Dns_queryA,Dns_queryA_len) ->

    io:format("**DNS** hostname:~p NOT found!~n",[Hostname]),

    _QueryA_response = dns_encode_queryA_header_and_question(Dns_id,
                                                   ?DNS_ANSWER_ZERO,
                                                         Dns_queryA,
                                                     Dns_queryA_len).

dns_decode_queryA_request(Socket,Hosts_by_name,Host,Port,Src_packet) ->

    % The request
    %
    <<Dns_id:?DNS_ID_LEN(),
      _Dns_flags_questions_answers_numauth_numadd:(?DNS_HEADER_LEN()-
                                                       ?DNS_ID_LEN()),
      Dns_rest_of_msg/binary>> = Src_packet,

    % Obtain the host name
    %
    <<Dns_hostname_len_bytes:?DNS_QUESTIONS_LEN(),
      _/binary>> = Dns_rest_of_msg,

    Dns_hostname_len = Dns_hostname_len_bytes*?BYTE,

    <<_len:?DNS_QUESTIONS_LEN(),
      Dns_hostname:Dns_hostname_len,_/binary>> = Dns_rest_of_msg,

    Hostname = binary_to_list(<<Dns_hostname:Dns_hostname_len>>),
    io:format("**DNS** queried: ~p~n",[Hostname]),

    Host_ips = maps:find(Hostname,Hosts_by_name),

    % Obtain the query A (host name + QTYPE + QCLASS))
    %
    Dns_queryA_len = ?DNS_QUESTIONS_LEN()+
                        Dns_hostname_len+
                    ?NULL_TERMINATION_LEN+
                         ?DNS_QTYPE_LEN()+
                        ?DNS_QCLASS_LEN(),

    <<Dns_queryA:Dns_queryA_len,
      _/binary>> = Dns_rest_of_msg,


    % The response
    %
    Dst_packet = dns_encode_queryA_response(Host_ips,
                                            Hostname,
                                              Dns_id,
                                          Dns_queryA,
                                     Dns_queryA_len),

    io:format("**DNS** Dst_packet:~p~n",[Dst_packet]),

    gen_udp:send(Socket, Host, Port, Dst_packet).

dns_receive(Socket,Hosts_by_name) ->
    receive
        {udp, Socket, Host, Port, Src_packet} = Src_Data ->
            io:format("**DNS** server received:~p~n",[Src_Data]),

            Fun = fun() -> dns_decode_queryA_request(Socket,
                                              Hosts_by_name,
                                                       Host,
                                                       Port,
                                            Src_packet) end,

            _Pid = spawn(Fun), % Spawns one process per query
            dns_receive(Socket,Hosts_by_name)
    end.

dns_server(Port,Hosts_by_name) ->
    {ok, Socket} = gen_udp:open(Port, [binary, {active,true}]),
    io:format("**DNS** server opened socket:~p~n",[Socket]),
    dns_receive(Socket,Hosts_by_name).

run(Port) ->

    % Code and configuration file in the same dir (MODULE trick)
    File = filename:join(filename:dirname(code:which(?MODULE)),"dns.hosts.txt"),

    io:format("**DNS** server port:~p file:~p~n",[Port,File]),

    Hosts_by_name = dns_get_hosts_by_name(File),

    % Start listening requests...
    io:format("**DNS** server configured with:~p~n",[Hosts_by_name]),

    dns_db_init(),
    dns_db_start(),
    dns_db_provision(Hosts_by_name),

    dns_server(Port,Hosts_by_name).

%%http://blog.rusty.io/2011/01/13/beautiful-erlang-print/
