set hidden
set noshowmode

" don't give |ins-completion-menu| messages.
set shortmess+=c

" Disable folds
set nofoldenable

" Better display for messages
set cmdheight=2

" Always show the last 5 lines
set scrolloff=5

" always show signcolumns
set signcolumn=yes
set clipboard=unnamedplus

" Use global replace by default (/g) to revert
set gdefault

" Set fixed popup menu height
set pumheight=8

if has('nvim')
  " Use pum as wildmenu
  " set wildoptions=pum
  " Enable incremental commands (e.g. search-replace preview)
  set inccommand=split
endif


" Enable automatic indentation
set autoindent 
filetype plugin indent on    " required

" Reload .vimrc on save
autocmd! bufwritepost .vimrc source %

" Autoinstall Plug if not available
if empty(glob('~/.vim/autoload/plug.vim'))
  silent execute "!curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

" Swap file
set directory=~/.vim/tmp

" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'

call plug#begin('~/.vim/plugged')
" Highlight words under cursor
Plug 'RRethy/vim-illuminate'

" Match-Up: vim matchit alternative
Plug 'andymass/vim-matchup'

" Disable search highligh when done searching
Plug 'romainl/vim-cool'

" Additional targets
Plug 'wellle/targets.vim'

" Bracket autocomplete
Plug 'jiangmiao/auto-pairs'

" Colorscheme
Plug 'rakr/vim-one'
Plug 'romgrk/doom-one.vim'
Plug 'arcticicestudio/nord-vim'

" Easier vim navigation
Plug 'easymotion/vim-easymotion'

" Quoting/paranthesizing made simple
Plug 'tpope/vim-surround'

" Comment/uncomment with gc
Plug 'tpope/vim-commentary'

" FZF
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Sensible vim config
Plug 'tpope/vim-sensible'

" Advanced syntax highlighting
Plug 'sheerun/vim-polyglot'

" Statusline
Plug 'itchyny/lightline.vim'

" Add repeat support for plugins
Plug 'tpope/vim-repeat'

" Github Copilot
" Plug 'github/copilot.vim'

" Modus theme
Plug 'miikanissi/modus-themes.nvim'

" Initialize plugin system
call plug#end()


" General vim config {{{
" Set leader key to <space> 
:let mapleader = ' '      " Leader key
:let maplocalleader = ',' " Local leader key

syntax on                 " Enable syntax highlighting

set termguicolors

" {{{ Colorscheme
set background=dark
colorscheme doom-one
" colorscheme modus

function! SetBackgroundFromDarkman()
    " Get the output of `darkman get`
    let theme = system('darkman get')
    " Remove any extra whitespace/newline characters
    let theme = trim(theme)

    " Set background according to the theme
    if theme == 'dark'
        set background=dark
    elseif theme == 'light'
        set background=dark
    endif
endfunction

" Call the function at Vim startup
" autocmd VimEnter * call SetBackgroundFromDarkman()
" }}}

" {{{ Native Editor Settings
set path=.,,**
set expandtab
set tabstop=4
set softtabstop=2
set shiftwidth=2
set number                " Enable numbers
set relativenumber        " Enable relative numbers
set showcmd               " Show command in bottom right position
set cursorline            " Highlight line where curser is
" set colorcolumn=100
set wildmenu
set showmatch
set incsearch             " Enable incremental seach; Highlight while typing
set ignorecase            " Ignore case for searches; temp undo with /\c or /\C
" }}}

" Allow for visual movements over wrapped lines
nnoremap j gj
nnoremap k gk

"split navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Keep visual mode while indenting
vnoremap < <gv
vnoremap > >gv
" }}}

" Lightline {{{
let g:lightline = {
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ], ['currentfunction','readonly', 'relativepath', 'modified'] ],
      \   'right' : [ ['lineinfo'], ['percent']]
      \ },
      \ 'colorscheme': 'one',
      \ 'component_function': {
      \ }
      \ }

let g:lightline.separator = {
      \   'left': '', 'right': ''
      \}
let g:lightline.subseparator = {
      \   'left': '', 'right': '' 
      \}
" }}}


" EasyMotion {{{
let g:EasyMotion_do_mapping = 0 " Disable default mappings
let g:EasyMotion_smartcase = 1
" }}}

" Disable trailing spaces warning
let g:python_highlight_space_errors=0

" Run make on <F9>
nnoremap <F9> :make<CR>

" Python bindings {{{
augroup pythonbindings
  autocmd! pythonbindings
  " Run python file
  autocmd Filetype python set makeprg=python3\ %
  
  " Set textwidth in python to 100 lines
  autocmd Filetype python set textwidth=100
  autocmd Filetype python set colorcolumn=101

  " Insert debugging with pdb
  autocmd Filetype python nnoremap <buffer> <silent> <localleader>b :call InsertPDB()<CR>

  " Python import
  autocmd Filetype python nnoremap <buffer> <silent> <localleader>i     :ImportName<CR>

  " Organize imports
  autocmd Filetype python nnoremap <buffer> <silent> <localleader>o :OR<CR>

  " autocmd Filetype python vnoremap <buffer> <silent> <localleader>d :'<,'>GenPyDoc<CR>
  " Function to insert python PDB debug line
  function! InsertPDB()
    let trace = expand("__import__(\"pdb\").set_trace()")
    execute "normal O".trace
  endfunction
augroup end
" }}}


" CTRL-C doesn't trigger the InsertLeave autocmd . map to <ESC> instead.
inoremap <c-c> <ESC>


" Delte backwards in insert mode
noremap! <C-BS> <C-w>
noremap! <M-BS> <C-w>
inoremap <C-w> <C-\><C-o>dB
inoremap <C-BS> <C-\><C-o>db


" Add surroundings in visual mode and insert
vmap s) S)i
vmap s( S)i
vmap s] S]i
vmap s[ S]i
vmap s{ S}i
vmap s} S}i

" Leader mappings {{
nnoremap <silent> <Leader>bb :Buffers<CR>
nnoremap <silent> <Leader>bd :bd<CR>
nnoremap <silent> <Leader>w/ :vsplit<CR>
nnoremap <silent> <Leader>w- :split<CR>
nnoremap <silent> <Leader>wl <C-W><C-L>
nnoremap <silent> <Leader>wj <C-W><C-J>
nnoremap <silent> <Leader>wk <C-W><C-K>
nnoremap <silent> <Leader>wh <C-W><C-H>
nnoremap <silent> <Leader>/ :Ag<CR>
nnoremap <silent> <Leader>sp :Lines<CR>
nnoremap <silent> <Leader>sb :BLines<CR>
nnoremap <silent> <Leader>tb :BTags<CR>
nnoremap <silent> <Leader>tp :Tags<CR>
nnoremap <silent> <Leader>ff :Files<CR>
nnoremap <silent> <Leader>h :History<CR>

" Jump motions
map sw <Plug>(easymotion-w)
map sb <Plug>(easymotion-b)
map sj <Plug>(easymotion-j)
map sk <Plug>(easymotion-k)
" }}

" Center after search {{{
nnoremap n nzz
nnoremap N Nzz
" }}}

" Vim cool{{{
let g:CoolTotalMatches = 1
" }}}


" {{{ Vim Illuminate 
" Time in milliseconds (default 250)
let g:Illuminate_delay = 250
let g:Illuminate_ftblacklist = ['nerdtree', 'python']
" }}}


" Transparent background {{{
" highlight Normal guibg=none
" highlight NonText guibg=none
" highlight Normal ctermbg=none
" highlight NonText ctermbg=none
" }}}
