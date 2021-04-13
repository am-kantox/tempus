defmodule Tempus.Telemetria do
  @moduledoc false

  defmacro __using__(opts) do
    if match?({:module, Telemetria}, Code.ensure_compiled(Telemetria)) do
      quote do
        use Telemetria, unquote(opts)
      end
    else
      quote do
        @before_compile Tempus.Telemetria
      end
    end
  end

  def __before_compile__(%Macro.Env{module: module}) do
    case Module.get_attribute(module, :telemetria) do
      [_ | _] ->
        # Mix.shell().info(
        #   ":telemetria events for #{inspect(module)} are disabled."
        # )
        :ok

      _ ->
        :ok
    end
  end
end
