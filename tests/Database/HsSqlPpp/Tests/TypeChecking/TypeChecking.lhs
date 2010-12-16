Type checking: first stage adds range qualifiers to every column
reference (aka correlation names/ qualifiers), and replaces * in
select lists and aggregates

Second stage actually works out the types.