Mox.Server.start_link([])

Mox.defmock(ElvenGard.FrontendMock, for: ElvenGard.Frontend)

Application.put_env(:elven_gard, :frontend_mod, ElvenGard.FrontendMock)
