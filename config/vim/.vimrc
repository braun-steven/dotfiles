set hidden
" Remove '-- INSERT --' line since it is shown in lighline anyway
set noshowmode
set clipboard=unnamed

" Reload .vimrc on save
autocmd! bufwritepost .vimrc source %

" Disable arrows
let g:elite_mode=1

" Enable hard time using hjkl
"let g:hardtime_default_on = 1


" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')
  Plug 'fvictorio/vim-extract-variable'
  Plug 'machakann/vim-highlightedyank'
  Plug '~/.vim/plugged/vim-pydoc'
  Plug 'morhetz/gruvbox'

  Plug 'terryma/vim-multiple-cursors'
  " Show marks
  Plug 'kshenoy/vim-signature'

  " Python autoimport
  Plug 'mgedmin/python-imports.vim'

  " Tex
  Plug 'lervag/vimtex'

  " Have a hard time using hjkl
  Plug 'takac/vim-hardtime'

  " Easier vim navigation
  Plug 'easymotion/vim-easymotion'

  " Quoting/paranthesizing made simple
  Plug 'tpope/vim-surround'

  " Vim git integration
  Plug 'tpope/vim-fugitive' " Git commands
  Plug 'tpope/vim-rhubarb' " Gbrowse support for github
  Plug 'shumphrey/fugitive-gitlab.vim' " Gbrowse support for gitlab
  Plug 'airblade/vim-gitgutter' " Git info on the sidebar
  Plug 'rhysd/committia.vim' " Git commit extension

  " Comment/uncomment with gc
  Plug 'tpope/vim-commentary'

  " Linting
  Plug 'w0rp/ale'

  " Python virtual envs
  Plug 'plytophogy/vim-virtualenv'

  " FZF
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
  Plug 'junegunn/fzf.vim'

  " Sensible vim config
  Plug 'tpope/vim-sensible'

  " Bash support
  Plug 'vim-scripts/bash-support.vim'

  " Tagbar with <F8>
  Plug 'majutsushi/tagbar'

  " CTag automation
  Plug 'ludovicchabant/vim-gutentags'

  " Bracket autocomplete
  Plug 'jiangmiao/auto-pairs'

  " Advanced syntax highlighting
  Plug 'sheerun/vim-polyglot'

  " Statusline
  Plug 'itchyny/lightline.vim'
  Plug 'mengelbrecht/lightline-bufferline'
  Plug 'maximbaz/lightline-ale'

  " Julia support
  Plug 'JuliaEditorSupport/julia-vim'

  " File tree with <C-n>
  Plug 'scrooloose/nerdtree'
  Plug 'Xuyuanp/nerdtree-git-plugin'

  "" Autocomplete framework
  " Plug 'Valloric/YouCompleteMe', { 'do': './install.py' }
  if has('nvim')
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
  else
    Plug 'Shougo/deoplete.nvim'
    Plug 'roxma/nvim-yarp'
    Plug 'roxma/vim-hug-neovim-rpc'
  endif
  Plug 'davidhalter/jedi-vim'
  Plug 'zchee/deoplete-jedi'
  Plug 'ervandew/supertab'
   
  " Hex color preview
  Plug 'lilydjwg/colorizer'

  " Language spell/grammer checker, invoke with: :LanguageToolCheck
  Plug 'dpelle/vim-LanguageTool'

  " UtilSnips
    " Track the engine.
  Plug 'SirVer/ultisnips'

  " Snippets are separated from the engine. Add this if you want them:
  Plug 'honza/vim-snippets'

  " Initialize plugin system
call plug#end()

filetype plugin indent on    " required

"Credit joshdick
"Use 24-bit (true-color) mode in Vim/Neovim when outside tmux.
"If you're using tmux version 2.2 or later, you can remove the outermost $TMUX check and use tmux's 24-bit color support
"(see < http://sunaku.github.io/tmux-24bit-color.html#usage > for more information.)
if (empty($TMUX))
  if (has("nvim"))
  "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
  endif
  "For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
  "Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
  " < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
  if (has("termguicolors"))
    set termguicolors
  endif
endif

" Set leader key to <space> 
:let mapleader = ' '

syntax on 
set background=dark
" let g:onedark_terminal_italics = 1
let g:gruvbox_italic = 1
colorscheme gruvbox 
set path=.,,**
set expandtab
set tabstop=4
set softtabstop=2
set shiftwidth=2
set number
set showcmd
set cursorline
set colorcolumn=120
set wildmenu
set showmatch
set incsearch
set relativenumber

" Map Ctrl-c to esc
" imap <C-c> <Esc>

"split navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

nnoremap H ^
nnoremap L $

" Lightline
let g:lightline = {
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ], [ 'gitbranch', 'readonly', 'relativepath', 'modified' ] ],
      \   'right' : [ ['lineinfo'], ['percent'], ['filetype'], [ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_ok' ]]
      \ },
      \ 'colorscheme': 'gruvbox',
      \ 'component_function': {
      \   'mode': 'LightlineMode',
      \   'gitbranch' : 'fugitive#head',
      \ }
      \ }

let g:lightline.component_expand = {
      \  'linter_checking': 'lightline#ale#checking',
      \  'linter_warnings': 'lightline#ale#warnings',
      \  'linter_errors': 'lightline#ale#errors',
      \  'linter_ok': 'lightline#ale#ok',
      \ }

let g:lightline.component_type = {
      \     'linter_checking': 'left',
      \     'linter_warnings': 'warning',
      \     'linter_errors': 'error',
      \     'linter_ok': 'left',
      \ }

let g:lightline.separator = {
	\   'left': '', 'right': ''
  \}
let g:lightline.subseparator = {
	\   'left': '', 'right': '' 
  \}


function! LightlineMode()
  return expand('%:t') ==# '__Tagbar__' ? 'Tagbar':
        \ expand('%:t') ==# 'ControlP' ? 'CtrlP' :
        \ &filetype ==# 'unite' ? 'Unite' :
        \ &filetype ==# 'vimfiler' ? 'VimFiler' :
        \ &filetype ==# 'vimshell' ? 'VimShell' :
        \ lightline#mode()
endfunction

" Lightline Buffers
set showtabline=2
let g:lightline#bufferline#show_number  = 0
let g:lightline#bufferline#shorten_path = 1
let g:lightline#bufferline#unnamed      = '[No Name]'
let g:lightline.tabline          = {'left': [['buffers']], 'right' : []}
let g:lightline.component_expand = {'buffers': 'lightline#bufferline#buffers'}
let g:lightline.component_type   = {'buffers': 'tabsel'}
autocmd BufWritePost,TextChanged,TextChangedI * call lightline#update()
nmap <Leader>1 <Plug>lightline#bufferline#go(1)
nmap <Leader>2 <Plug>lightline#bufferline#go(2)
nmap <Leader>3 <Plug>lightline#bufferline#go(3)
nmap <Leader>4 <Plug>lightline#bufferline#go(4)
nmap <Leader>5 <Plug>lightline#bufferline#go(5)
nmap <Leader>6 <Plug>lightline#bufferline#go(6)
nmap <Leader>7 <Plug>lightline#bufferline#go(7)
nmap <Leader>8 <Plug>lightline#bufferline#go(8)
nmap <Leader>9 <Plug>lightline#bufferline#go(9)
nmap <Leader>0 <Plug>lightline#bufferline#go(10)
" Map file tree and tab bar to <F5> and <F6>
map <F5> :NERDTreeToggle<CR>
map <F6> :TagbarToggle<CR>
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" Add julia tagbar config
let g:tagbar_type_julia = {
    \ 'ctagstype' : 'julia',
    \ 'kinds'     : [
        \ 't:struct', 'f:function', 'm:macro', 'c:const']
    \ }

" FZF
nmap ; :Buffers<CR>
nmap <Leader><Leader>b :Buffers<CR>
nmap <Leader><Leader>l :Lines<CR>
nmap <Leader><Leader>t :BTags<CR>
nmap <Leader><Leader>T :Tags<CR>
nmap <Leader><Leader>f :Files<CR>
nmap <Leader><Leader>h :History<CR>

" Customize fzf colors to match your color scheme
autocmd! FileType fzf
autocmd  FileType fzf set laststatus=0 noshowmode noruler
  \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler
let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'border':  ['fg', 'Ignore'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }


" Disable arrow movement, resize splits instead.
if get(g:, 'elite_mode')
	nnoremap <Up>    :resize +2<CR>
	nnoremap <Down>  :resize -2<CR>
	nnoremap <Left>  :vertical resize +2<CR>
	nnoremap <Right> :vertical resize -2<CR>
endif

" Ale fixers
let g:ale_fixers = ['black'] 
let g:ale_fix_on_save = 1

" Clear search
nnoremap <CR> :noh<CR><CR>

" Jedi
let g:jedi#rename_command = "<leader>r"
let g:jedi#auto_close_doc = 1
let g:jedi#usages_command = '<Leader>u'
let g:jedi#goto_command = "gd"
let g:jedi#show_call_signatures = "1"
" Disable since deoplete is enabled
let g:jedi#completions_enabled = 0

" EasyMotion
map  <Leader>f <Plug>(easymotion-bd-f)
" Move to line
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
" Move to word
map  <Leader>w <Plug>(easymotion-bd-w)
let g:EasyMotion_smartcase = 1

" Quick scope
" Trigger a highlight in the appropriate direction when pressing these keys:
let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']

" Disable trailing spaces warning
let g:python_highlight_space_errors=0

" ALE config
let g:ale_set_highlights = 1
" highlight ALEWarning gui=undercurl guisp=#fac863
" highlight ALEError gui=undercurl guisp=#ec5f67
" Check Python files with flake8 and pylint.
let b:ale_linters = ['flake8', 'pylint']
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
" Lint always in Normal Mode
let g:ale_lint_on_text_changed = 'normal'
" Lint when leaving Insert Mode but don't lint when in Insert Mode 
let g:ale_lint_on_insert_leave = 1

" Disable latex-box from polyglot dependency to make vimtex usable
let g:polyglot_disabled = ['latex']

" Python import
map <F10>    :ImportName<CR>

" Deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_ignore_case = 1
let g:deoplete#enable_smart_case = 1
" Disable autocompletion (using deoplete instead)
let g:jedi#completions_enabled = 0
"set completeopt-=preview
let g:python_host_prog = '/usr/bin/python'
let g:python3_host_prog = '/usr/bin/python3'

" " use tab to forward cycle
" inoremap <silent><expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"
" " use tab to backward cycle
" inoremap <silent><expr><s-tab> pumvisible() ? "\<c-p>" : "\<s-tab>"
" inoremap <silent><expr> <TAB>
" 		\ pumvisible() ? "\<C-n>" :
" 		\ <SID>check_back_space() ? "\<TAB>" :
" 		\ deoplete#mappings#manual_complete()
" 		function! s:check_back_space() abort "{{{
" 		let col = col('.') - 1
" 		return !col || getline('.')[col - 1]  =~ '\s'
" 		endfunction"}}}
"

" Reformat python code with \r
nmap \r :ALEFix black<CR>
nmap \i :ALEFix isort<CR>

" LanguageTool config
let g:languagetool_jar = '$HOME/Downloads/LanguageTool-4.3/languagetool-commandline.jar'
hi LanguageToolGrammarError  guisp=#b58900 gui=undercurl guifg=NONE guibg=NONE ctermfg=white ctermbg=blue term=underline cterm=none
hi LanguageToolSpellingError guisp=#dc322f  gui=undercurl guifg=NONE guibg=NONE ctermfg=white ctermbg=red  term=underline cterm=none

" UtilSnips
" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
  let g:UltiSnipsExpandTrigger="<tab>"
  let g:UltiSnipsJumpForwardTrigger="<c-b>"
  let g:UltiSnipsJumpBackwardTrigger="<c-z>"

  " If you want :UltiSnipsEdit to split your window.
  let g:UltiSnipsEditSplit="vertical"

" Supertab 
let g:SuperTabDefaultCompletionType = "<c-n>"

" Map jj, jk, kj to exit insert mode
imap jj <Esc>
imap jk <Esc>
imap kj <Esc>

" CTRL-C doesn't trigger the InsertLeave autocmd . map to <ESC> instead.
inoremap <c-c> <ESC>

" italics fix
" set t_ZH=^[[3m
" set t_ZR=^[[23m

map \b :call InsertLine()<CR>

function! InsertLine()
  let trace = expand("import ipdb; ipdb.set_trace(context=5)")
  execute "normal O".trace
endfunction

" Delte backwards in insert mode
noremap! <C-BS> <C-w>
noremap! <M-BS> <C-w>


inoremap <C-w> <C-\><C-o>dB
inoremap <C-BS> <C-\><C-o>db


" Ultisnips
let g:ultisnips_python_style="google"

" Add surroundings in visual mode and insert
vmap s) S)i
vmap s( S)i
vmap s] S]i
vmap s[ S]i
vmap s{ S}i
vmap s} S}i

" Python print stuff in selection
vnoremap p yoprint("<ESC>pa:", <ESC>pa)<ESC>
