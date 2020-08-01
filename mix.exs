defmodule ElvenGard.MixProject do
  use Mix.Project

  @app_name "ElvenGard"
  @version "0.1.0-alpha"
  @github_link "https://github.com/ImNotAVirus/ElvenGard_V2"

  def project do
    [
      app: :elven_gard,
      version: @version,
      elixir: "~> 1.7",
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
      extra_applications: [:logger]
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
      {:elixir_uuid, "~> 1.2"},
      {:ex_doc, "~> 0.21", only: :dev, runtime: false},
      {:dialyxir, "~> 1.0", optional: true, only: [:dev, :test], runtime: false},
      {:credo, "~> 1.1", optional: true, only: [:dev, :test], runtime: false},
      {:inch_ex, "~> 2.0", optional: true, only: [:dev, :test], runtime: false},
      {:excoveralls, "~> 0.11", only: :test, runtime: false}
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
      groups_for_extras: groups_for_extras(),
      groups_for_modules: [
        "Textual protocol specs": ~r/ElvenGard\.Protocol\.Textual\.?/,
        "Binary protocol specs": ~r/ElvenGard\.Protocol\.Binary\.?/,
        Structures: ~r/ElvenGard\.Structures\./
      ]
    ]
  end

  defp dialyzer() do
    [
      plt_add_apps: [:elven_gard],
      plt_add_deps: :apps_direct,
      flags: [
        :unmatched_returns,
        :error_handling,
        :race_conditions,
        :no_opaque,
        :unknown,
        :no_return
      ]
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
