local lspconfig = require("lspconfig")
local conform = require("conform")

lspconfig.pyright.setup({})
conform.formatters_by_ft.python = { "ruff_format" }
