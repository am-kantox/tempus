defmodule Tempus.Telemetria do
  @moduledoc false

  {available?, enabled?, compiler?} =
    {
      match?({:module, Telemetria}, Code.ensure_compiled(Telemetria)),
      Application.compile_env(:tempus, :telemetria?, false),
      :telemetria in Mix.compilers()
    }

  defmacro __using__(opts), do: do_using(__CALLER__, opts)

  # @dialyzer {:nowarn_function, do_using: 2}

  case [enabled?, available?, compiler?] do
    [true, false, _] ->
      defp do_using(caller, _opts) do
        raise CompileError,
          file: caller.file,
          line: caller.line,
          description:
            ":telemetria has been enabled but it’s not available, please include it into `deps`"
      end

    [true, true, compiler?] ->
      defp do_using(_caller, opts) do
        if not unquote(compiler?) do
          IO.warn(
            ":telemetria has been enabled for `Tempus` but the compiler is not specified, " <>
              "please include it into `compilers:` list in your `project` callback",
            []
          )
        end

        quote do
          use Telemetria, unquote(opts)
        end
      end

    [false, _, _] ->
      defp do_using(_caller, _opts) do
        quote do
          @before_compile Tempus.Telemetria
        end
      end
  end

  def __before_compile__(%Macro.Env{module: module}) do
    case Module.get_attribute(module, :telemetria) do
      [_ | _] ->
        # Mix.shell().info(":telemetria events for #{inspect(module)} are disabled.")
        :ok

      _ ->
        :ok
    end
  end
end
