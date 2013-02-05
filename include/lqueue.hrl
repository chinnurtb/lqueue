-type length()     :: non_neg_integer().

-type max_length() :: pos_integer().

-type rear_list()  :: list().

-type front_list() :: list().

%% lqueue type

-type lqueue()     :: {length(), max_length(), rear_list(), front_list()}.
