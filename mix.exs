defmodule ElvenGard.MixProject do
  use Mix.Project

  @version "0.1.0-alpha"

  def project do
    [
      app: :elven_gard,
      version: @version,
      elixir: "~> 1.7",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      description: description(),
      package: package(),
      deps: deps(),
      name: "ElvenGard",
      source_url: "https://github.com/ImNotAVirus/ElvenGard_V2",
      # homepage_url: "https://YOUR_PROJECT_HOMEPAGE",
      docs: docs()
    ]
  end

  defp package do
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
        Changelog: "https://github.com/ImNotAVirus/ElvenGard_V2/blob/master/CHANGELOG.md",
        GitHub: "https://github.com/ImNotAVirus/ElvenGard_V2"
      }
    ]
  end

  defp description() do
    "MMORPG Game Server toolkit written in Elixir"
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:ranch, "~> 1.5"},
      {:elixir_uuid, "~> 1.2"},
      {:ex_doc, "~> 0.19.0", only: :dev, runtime: false},
      {:credo, "~> 1.0.0", only: [:dev, :test], runtime: false}
    ]
  end

  defp docs() do
    [
      # The main page in the docs
      main: "ElvenGard",
      # logo: "path/to/logo.png",
      source_ref: "v#{@version}",
      extras: ["README.md"],
      groups_for_modules: [
        "Helpers": ~r/^ElvenGard.Helpers.?/,
        "Structures": ~r/^ElvenGard.Structures.?/
      ]
    ]
  end
end
