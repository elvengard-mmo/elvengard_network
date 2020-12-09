defmodule ElvenGard.MixProject do
  use Mix.Project

  @app_name "ElvenGard"
  @version "0.1.0-alpha"
  @github_link "https://github.com/ImNotAVirus/elvengard-network"

  def project do
    [
      app: :elven_gard,
      version: @version,
      elixir: "~> 1.8",
      deps: deps(),
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      consolidate_protocols: Mix.env() != :test,

      # Docs
      name: @app_name,
      docs: docs(),

      # Testing
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test
      ],

      # Dialyzer
      dialyzer: dialyzer(),

      # Hex
      description: description(),
      package: package()
    ]
  end

  def application() do
    [
      extra_applications: [:logger, :crypto]
    ]
  end

  defp package() do
    [
      description: description(),
      files: [
        "lib",
        "mix.exs",
        "README.md",
        "CHANGELOG.md",
        "COPYING*",
        ".formatter.exs"
      ],
      maintainers: ["ImNotAVirus"],
      licenses: ["AGPL", "LGPL"],
      links: %{
        # Website: "https://YOUR_PROJECT_WEBSITE",
        Changelog: "#{@github_link}/blob/master/CHANGELOG.md",
        GitHub: @github_link
      }
    ]
  end

  defp description() do
    "MMORPG Game Server toolkit written in Elixir"
  end

  defp deps() do
    [
      {:ranch, "~> 2.0"},
      {:nimble_parsec, "~> 1.1"},
      {:mox, "~> 1.0", only: :test, runtime: false},
      {:ex_doc, "~> 0.23", only: :dev, runtime: false},
      {:dialyxir, "~> 1.0", optional: true, only: [:dev, :test], runtime: false},
      {:credo, "~> 1.5", optional: true, only: [:dev, :test], runtime: false},
      {:excoveralls, "~> 0.13", only: :test, runtime: false}
    ]
  end

  defp docs() do
    [
      main: @app_name,
      source_ref: "v#{@version}",
      source_url: @github_link,
      # logo: "path/to/logo.png",
      extra_section: "GUIDES",
      extras: extras(),
      groups_for_extras: groups_for_extras()
      # groups_for_modules: [
      #   "Textual protocol specs": ~r/ElvenGard\.Protocol\.Textual\.?/,
      #   "Binary protocol specs": ~r/ElvenGard\.Protocol\.Binary\.?/,
      #   PacketHandler: ~r/ElvenGard\.PacketHandler\./
      # ]
    ]
  end

  defp dialyzer() do
    [
      plt_core_path: "priv/plts",
      plt_file: {:no_warn, "priv/plts/dialyzer.plt"},
      # plt_add_apps: [:elven_gard],
      plt_add_deps: :app_tree
      # flags: [
      #   :no_return,
      #   :no_unused,
      #   :no_improper_lists,
      #   :no_fun_app,
      #   :no_match,
      #   :no_opaque,
      #   :no_fail_call,
      #   :no_contracts,
      #   :no_behaviours,
      #   :no_undefined_callbacks,
      #   :unmatched_returns,
      #   :error_handling,
      #   :race_conditions
      # ]
    ]
  end

  defp extras() do
    ["README.md": [title: "Overview"]] ++ Path.wildcard("guides/**/*.md")
  end

  defp groups_for_extras() do
    [
      Introduction: ~r/(README.md|guides\/introduction\/.?)/,
      Topics: ~r/guides\/topics\/.?/
    ]
  end
end
