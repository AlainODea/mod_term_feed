%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2011 Alain O'Dea
%% @date 2011-02-06
%% @doc Serve an external term feed for a category.  Shows the newest created
%% items of a certain category, always as the anonymous user.
%% The category being served must be a name.
%% Largely copied from Marc Worrell's resource_atom_feed_cat.erl

%% Copyright 2011 Alain O'Dea
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(resource_term_feed_cat).
-author("Alain O'Dea <alain.odea@gmail.com>").

-export([
    init/1,
    service_available/2,
    allowed_methods/2,
    encodings_provided/2,
	resource_exists/2,
	last_modified/2,
	expires/2,
	content_types_provided/2,
	provide_content/2
]).

-include_lib("webmachine_resource.hrl").
-include_lib("include/zotonic.hrl").

%% Let cached versions expire in an hour.
-define(MAX_AGE, 3600).


init(DispatchArgs) ->
    {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    Context2 = z_context:ensure_qs(Context1),
    ?WM_REPLY(true, Context2).


allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET'], ReqData, Context}.


encodings_provided(ReqData, Context) ->
    {[
        {"identity", fun(X) -> X end}, 
        {"gzip", fun(X) -> zlib:gzip(X) end}
    ], ReqData, Context}.


content_types_provided(ReqData, Context) ->
    {[{"application/octet-stream", provide_content}], ReqData, Context}.


resource_exists(ReqData, Context) ->
    {m_rsc:exists(z_context:get_q(cat, Context), Context), ReqData, Context}.


last_modified(ReqData, Context) ->
    RD1 = wrq:set_resp_header("Cache-Control", "public, max-age="++integer_to_list(?MAX_AGE), ReqData),
    Modified = case m_category:last_modified(z_context:get_q(cat, Context), Context) of
        {ok, Date} -> Date;
        {error, _Reason} -> {{2008,12,10},{15,30,00}}
    end,
    Context1 = z_context:set(last_modified, Modified, Context),
    {Modified, RD1, Context1}.


expires(ReqData, State) ->
    NowSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    {calendar:gregorian_seconds_to_datetime(NowSecs + ?MAX_AGE), ReqData, State}.


provide_content(ReqData, Context) ->
    Context1 = z_context:set_reqdata(ReqData, Context),
    CatName = m_category:id_to_name(z_context:get_q(cat, Context1), Context1),
    Type = case z_context:get(upcoming, Context1) of
        true -> upcoming;
        _ -> latest
    end,
    F = fun() ->
        #search_result{result=Result} = z_search:search({Type, [{cat,CatName}]}, Context1),
        terms(Result, Context1)
    end,
    Content = {ok, F()}, %%z_depcache:memo(F, {atom_feed, CatName}, ?MAX_AGE, [CatName], Context1),
    ?WM_REPLY(term_to_binary(Content), Context1).


terms([], _Context) -> [];
terms([Id|Ids], Context) -> [{Id, m_rsc:get(Id, Context)}|terms(Ids, Context)].


