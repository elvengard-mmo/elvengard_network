defmodule ElvenGardV2.MixProject do
  use Mix.Project

  def project do
    [
      apps_path: "apps",
      start_permanent: Mix.env() == :prod,
      deps: deps(),

      # Docs
      name: "ElvenGard",
      source_url: "https://github.com/ImNotAVirus/ElvenGard_V2",
      homepage_url: "http://YOUR_PROJECT_HOMEPAGE",
      docs: [
        # The main page in the docs
        main: "ElvenGard",
        # logo: "path/to/logo.png",
        extras: ["README.md"]
      ]
    ]
  end

  # Dependencies listed here are available only for this
  # project and cannot be accessed from applications inside
  # the apps folder.
  #
  # Run "mix help deps" for examples and options.
  defp deps do
    [
      {:ex_doc, "~> 0.19.0", only: :dev, runtime: false}
    ]
  end
end
