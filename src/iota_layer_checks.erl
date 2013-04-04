-module(iota_layer_checks).

-export([ enforce_single_layer/2 ]).

enforce_single_layer({Module, Info}, Results) ->
  case iota_utils:get(layer, Info) of
    [Layer] when is_atom(Layer) -> Results;
    [_BadLayer]                 ->
      iota_errors:emit_error(Results,
                             Module,
                             {layer, invalid_layer_declaration});
    [_|_]                       ->
      iota_errors:emit_error(Results,
                             Module,
                             {layer, too_many_layers})
  end.
