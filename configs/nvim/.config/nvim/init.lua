local vimrc = vim.fn.stdpath("config") .. "/vimrc.vim"
print(vimrc)
vim.cmd.source(vimrc)
