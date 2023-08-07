defmodule DSL do
  defmacro deffunction({:when, _, [name, guards]} = a) do
    # # IO.inspect(__CALLER__, label: "__CALLER__")
    # IO.inspect(guards, label: "guards")
    # IO.inspect(function_exported?(__CALLER__.module, :__info__, 1))

    # IO.inspect(:elixir_utils.extract_guards(a), label: "AAAA")
    # IO.inspect(Macro.decompose_call(guards), label: "BBBB")

    # IO.inspect(:elixir_locals.cache_env(__CALLER__), label: "CCCC")
    # # :elixir_def.take_definition()

    env = Map.put(__CALLER__, :context, :env)
    
    guards
    |> IO.inspect()
    |> Macro.expand_once(env)
    # |> Macro.prewalk(&Macro.expand(&1, __CALLER__))
    # |> :elixir_expand.expand(:elixir_env.env_to_ex(__CALLER__), __CALLER__)
    |> Macro.to_string()
    |> IO.puts()

    # |> IO.inspect()

    quote do
      defmodule CustomModule do
        def unquote(name)(var!(arg1)) when unquote(guards) do
          IO.inspect(:ok)
        end
      end
    end
  end
end

defmodule MyApp2 do
  defguard is_true(value) when value == "1"

  defmacro test2(arg1) do
    quote do
      arg1 == 123
    end
  end
end

defmodule MyApp3 do
  defmacro test3 do
    quote do
      3
    end
  end
end

defmodule MyApp do
  import DSL, only: [deffunction: 1]
  import MyApp2
  require MyApp3

  defguard is_true(value) when value == "1"

  deffunction :my_function when is_true(arg1)
  # deffunction :my_function2 when arg1.test == 3
  # deffunction :my_function3 when test2(arg1)
  # deffunction :my_function4 when MyApp3.test3 == 3
end

MyApp.CustomModule.my_function("1")
