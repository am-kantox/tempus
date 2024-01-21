defmodule Tempus.MixProject do
  use Mix.Project

  @app :tempus
  @version "0.13.1"

  def project do
    [
      app: @app,
      version: @version,
      name: "Tempus",
      elixir: "~> 1.14",
      compilers: compilers(Mix.env()),
      elixirc_paths: elixirc_paths(Mix.env()),
      consolidate_protocols: Mix.env() not in [:dev, :test],
      preferred_cli_env: [quality: :ci, "quality.ci": :ci],
      description: description(),
      package: package(),
      deps: deps(),
      aliases: aliases(),
      xref: [exclude: []],
      docs: docs(),
      releases: [],
      dialyzer: [
        flags: ["-Wunmatched_returns", :error_handling, :underspecs],
        plt_file: {:no_warn, ".dialyzer/dialyzer.plt"},
        plt_add_deps: :app_tree,
        plt_add_apps: [:mix],
        list_unused_filters: true,
        ignore_warnings: ".dialyzer/ignore.exs"
      ],
      preferred_cli_env: [coveralls: :test, "coveralls.github": :test],
      test_coverage: [tool: ExCoveralls]
    ]
  end

  def application,
    do: [
      extra_applications: [:logger]
    ]

  defp deps do
    [
      {:telemetria, "~> 0.8", optional: true},
      {:avl_tree, "~> 1.0"},
      # dev / test
      {:tzdata, "~> 1.0", only: [:dev, :test]},
      {:benchee, "~> 1.0", only: [:dev, :ci]},
      {:credo, "~> 1.0", only: [:dev, :ci]},
      {:dialyxir, "~> 1.0", only: [:dev, :ci], runtime: false},
      {:excoveralls, "~> 0.13", only: [:test]},
      {:ex_doc, "~> 0.11", only: :dev}
    ]
  end

  defp aliases do
    [
      quality: ["format", "credo --strict", "dialyzer"],
      "quality.ci": [
        "format --check-formatted",
        "credo --strict",
        "dialyzer"
      ]
    ]
  end

  defp description do
    """
    Easy handling of time periods, like business days, holidays, etc.
    """
  end

  defp package do
    [
      name: @app,
      files: ~w|stuff lib mix.exs README.md LICENSE|,
      maintainers: ["Aleksei Matiushkin"],
      licenses: ["Kantox LTD"],
      links: %{
        "GitHub" => "https://github.com/am-kantox/#{@app}",
        "Docs" => "https://hexdocs.pm/#{@app}"
      }
    ]
  end

  defp docs do
    [
      main: "getting-started",
      source_ref: "v#{@version}",
      canonical: "http://hexdocs.pm/#{@app}",
      logo: "stuff/#{@app}-48x48.png",
      source_url: "https://github.com/am-kantox/#{@app}",
      assets: "stuff/images",
      extras: ~w[README.md stuff/background.md stuff/getting-started.md],
      groups_for_modules: [
        Helpers: [Tempus.Guards, Tempus.Sigils],
        Core: [Tempus.Slot, Tempus.Slots],
        Implementations: [Tempus.Slots.List, Tempus.Slots.Stream]
      ]
    ]
  end

  defp compilers(:test), do: Mix.compilers()
  defp compilers(:ci), do: Mix.compilers()

  defp compilers(_) do
    if Application.get_env(:tempus, :telemetria?, false) do
      [:telemetria | Mix.compilers()]
    else
      Mix.compilers()
    end
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(:dev), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]
end
