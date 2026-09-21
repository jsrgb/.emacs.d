vim.g.mapleader = " "
vim.g.maplocalleader = " "
vim.g.editorconfig = true

-- Editing
vim.opt.expandtab = true
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4
vim.opt.tabstop = 4
vim.opt.smartindent = true
vim.opt.undofile = true
vim.opt.confirm = true
vim.opt.clipboard = "unnamedplus"

-- Finding and completion
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.inccommand = "split"
vim.opt.completeopt = { "menu", "menuone", "noselect" }
vim.opt.pumheight = 10

-- Windows and display
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.cursorline = true
vim.opt.signcolumn = "yes"
vim.opt.scrolloff = 4
vim.opt.sidescrolloff = 4
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.termguicolors = true
vim.opt.wrap = false
vim.opt.mouse = "a"
vim.opt.updatetime = 250
vim.opt.timeoutlen = 400

-- Use common web defaults when a project does not provide .editorconfig.
-- Neovim applies EditorConfig after FileType autocommands, so project settings win.
vim.api.nvim_create_autocmd("FileType", {
  pattern = {
    "javascript",
    "javascriptreact",
    "json",
    "jsonc",
    "typescript",
    "typescriptreact",
  },
  callback = function()
    vim.opt_local.expandtab = true
    vim.opt_local.shiftwidth = 2
    vim.opt_local.softtabstop = 2
    vim.opt_local.tabstop = 2
  end,
})

-- nvim 0.12 package manager
vim.pack.add({
  { src = "https://github.com/tpope/vim-rsi" },
  { src = "https://github.com/nvim-mini/mini.nvim", version = "stable" },
  { src = "https://github.com/ibhagwan/fzf-lua" },
  { src = "https://github.com/nvim-treesitter/nvim-treesitter" },
  { src = "https://github.com/neovim/nvim-lspconfig" },
  { src = "https://github.com/hrsh7th/nvim-cmp" },
  { src = "https://github.com/hrsh7th/cmp-nvim-lsp" },
  { src = "https://github.com/phha/zenburn.nvim" },
  { src = "https://github.com/folke/which-key.nvim" },
})

require("zenburn").setup()

-- Keep the number column visually continuous with the editor.
vim.api.nvim_set_hl(0, "LineNr", { fg = "#5d6262", bg = "#3f3f3f" })
vim.api.nvim_set_hl(0, "CursorLineNr", { fg = "#d2d39b", bg = "#434443" })

local which_key = require("which-key")
which_key.setup({
  preset = "modern",
  icons = { mappings = false },
  triggers = {
    { "<leader>", mode = { "n", "v" } },
    { "g", mode = { "n", "v" } },
  },
})
which_key.add({
  { "<leader>f", group = "find" },
  { "<leader>l", group = "lsp" },
  { "gr", group = "lsp" },
})

-- vim-rsi intentionally leaves <C-k> to Vim's digraph command. Fill in the
-- Emacs behavior: kill to end of line, or kill the newline at end of line.
vim.keymap.set("i", "<C-k>", function()
  if vim.fn.col(".") > #vim.api.nvim_get_current_line() then
    return "<C-o>gJ"
  end
  return "<C-o>D"
end, { expr = true, desc = "Kill to end of line" })

vim.keymap.set("c", "<C-k>", function()
  local remaining = #vim.fn.getcmdline() - vim.fn.getcmdpos() + 1
  return string.rep("<Del>", math.max(remaining, 0))
end, { expr = true, desc = "Kill to end of command line" })

-- FZF
local fzf = require("fzf-lua")
fzf.setup({})
fzf.register_ui_select()

vim.keymap.set("n", "<C-;>", fzf.buffers,
  { desc = "Fuzzy buffers" })
vim.keymap.set("n", "<leader><leader>", fzf.buffers,
  { desc = "Fuzzy buffers" })
vim.keymap.set("n", "<leader>ff", fzf.files,
  { desc = "Find files" })
vim.keymap.set("n", "<leader>fg", fzf.live_grep,
  { desc = "Live grep" })
vim.keymap.set("n", "<leader>fb", fzf.buffers,
  { desc = "Fuzzy buffers" })
vim.keymap.set("n", "<leader>fh", fzf.helptags,
  { desc = "Find help" })
vim.keymap.set("n", "<leader>fr", fzf.resume,
  { desc = "Resume picker" })
vim.keymap.set("n", "<leader>fq", fzf.quickfix,
  { desc = "Fuzzy quickfix" })

-- Preserve the search bindings from the Emacs config.
vim.keymap.set("n", "<M-p>", fzf.files,
  { desc = "Find files" })
vim.keymap.set("n", "<M-F>", fzf.live_grep,
  { desc = "Live grep" })
vim.keymap.set("n", "<M-s>", fzf.blines,
  { desc = "Search current buffer" })

-- Completion
local cmp = require("cmp")
cmp.setup({
  completion = {
    autocomplete = false,
  },
  snippet = {
    expand = function(args)
      vim.snippet.expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert({
    ["<CR>"] = cmp.mapping.confirm({ select = true }),
  }),
  sources = {
    { name = "nvim_lsp" },
  },
})

vim.keymap.set("n", "<C-n>",
  "a<Cmd>lua require('cmp').complete()<CR>",
  { desc = "Enter Insert mode and show completion suggestions" })

require("mini.move").setup({
  mappings = {
    left = "",
    right = "",
    down = "<M-Down>",
    up = "<M-Up>",
    line_left = "",
    line_right = "",
    line_down = "<M-Down>",
    line_up = "<M-Up>",
  },
})

-- Tree-sitter parsers and highlighting for the languages in active use.
local treesitter = require("nvim-treesitter")
local treesitter_languages = {
  "javascript",
  "json",
  "lua",
  "rust",
  "tsx",
  "typescript",
  "vim",
  "vimdoc",
}

treesitter.install(treesitter_languages)

vim.api.nvim_create_autocmd("FileType", {
  pattern = {
    "javascript",
    "javascriptreact",
    "json",
    "jsonc",
    "lua",
    "rust",
    "typescript",
    "typescriptreact",
    "vim",
  },
  callback = function()
    pcall(vim.treesitter.start)
  end,
})

-- LSP
vim.lsp.config("*", {
  capabilities = require("cmp_nvim_lsp").default_capabilities(),
})
vim.lsp.enable({ "rust_analyzer", "tsc" })

vim.diagnostic.config({
  severity_sort = true,
  signs = {
    text = {
      [vim.diagnostic.severity.ERROR] = "●",
      [vim.diagnostic.severity.WARN] = "●",
      [vim.diagnostic.severity.INFO] = "●",
      [vim.diagnostic.severity.HINT] = "●",
    },
  },
})

local format_group = vim.api.nvim_create_augroup("user.lsp-format", {})

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(event)
    local client = assert(vim.lsp.get_client_by_id(event.data.client_id))
    local map_opts = { buffer = event.buf }

    vim.keymap.set("n", "gd", vim.lsp.buf.definition,
      vim.tbl_extend("force", map_opts, { desc = "Go to definition" }))
    vim.keymap.set("n", "gD", vim.lsp.buf.declaration,
      vim.tbl_extend("force", map_opts, { desc = "Go to declaration" }))
    vim.keymap.set("n", "grr", fzf.lsp_references,
      vim.tbl_extend("force", map_opts, { desc = "References with preview" }))
    vim.keymap.set("n", "K", vim.lsp.buf.hover,
      vim.tbl_extend("force", map_opts, { desc = "Hover documentation" }))
    vim.keymap.set("n", "<leader>la", vim.lsp.buf.code_action,
      vim.tbl_extend("force", map_opts, { desc = "Code action" }))
    vim.keymap.set("n", "<leader>lr", vim.lsp.buf.rename,
      vim.tbl_extend("force", map_opts, { desc = "Rename symbol" }))
    vim.keymap.set("n", "<leader>lf", function()
      vim.lsp.buf.format({ bufnr = event.buf })
    end, vim.tbl_extend("force", map_opts, { desc = "Format buffer" }))
    vim.keymap.set("n", "<M-O>", fzf.lsp_document_symbols,
      vim.tbl_extend("force", map_opts, { desc = "Document symbols" }))

    if client:supports_method("textDocument/formatting") then
      vim.api.nvim_clear_autocmds({ group = format_group, buffer = event.buf })
      vim.api.nvim_create_autocmd("BufWritePre", {
        group = format_group,
        buffer = event.buf,
        callback = function()
          vim.lsp.buf.format({
            bufnr = event.buf,
            id = client.id,
            timeout_ms = 2000,
          })
        end,
      })
    end
  end,
})

-- Keep searches highlighted until Escape clears them.
vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")

-- Familiar shortcuts from the Emacs config, without changing Insert mode.
vim.keymap.set("n", "<M-o>", "<C-w>w", { desc = "Next window" })
vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float,
  { desc = "Show diagnostic" })

vim.api.nvim_create_autocmd("TextYankPost", {
  desc = "Briefly highlight yanked text",
  callback = function()
    vim.hl.on_yank()
  end,
})
