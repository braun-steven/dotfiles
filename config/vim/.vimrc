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
  " Show marks
  Plug 'kshenoy/vim-signature'

  " Python autoimport
  Plug 'mgedmin/python-imports.vim'

  " Tex
  Plug 'lervag/vimtex'

  " Have a hard time using hjkl
  Plug 'takac/vim-hardtime'

  " Main theme
  Plug 'mhartington/oceanic-next'

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
  " Plug 'ervandew/supertab'

  " Hex color preview
  Plug 'lilydjwg/colorizer'

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

" syntax on 
set background=dark
let g:oceanic_next_terminal_bold = 1
let g:oceanic_next_terminal_italic = 1
colorscheme OceanicNext
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

" Map Ctrl-c to esc
" imap <C-c> <Esc>

"split navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Lightline
let g:lightline = {
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ], [ 'gitbranch', 'readonly', 'relativepath', 'modified' ] ],
      \   'right' : [ ['lineinfo'], ['percent'], ['filetype'] ]
      \ },
      \ 'colorscheme': 'oceanicnext',
      \ 'component_function': {
      \   'mode': 'LightlineMode',
      \   'gitbranch' : 'fugitive#head',
      \ }
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
nmap <Leader><Leader>b :Buffers<CR>
nmap <Leader><Leader>t :Tags<CR>
nmap <Leader><Leader>f :Files<CR>

" Disable ycm extra conf question
let g:ycm_confirm_extra_conf = 0

" Disable arrow movement, resize splits instead.
if get(g:, 'elite_mode')
	nnoremap <Up>    :resize +2<CR>
	nnoremap <Down>  :resize -2<CR>
	nnoremap <Left>  :vertical resize +2<CR>
	nnoremap <Right> :vertical resize -2<CR>
endif

" Ale fixers
let g:ale_fixers = ['prettier', 'standard'] 

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
highlight ALEWarning gui=undercurl guisp=#fac863
highlight ALEError gui=undercurl guisp=#ec5f67
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

" use tab to forward cycle
inoremap <silent><expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"
" use tab to backward cycle
inoremap <silent><expr><s-tab> pumvisible() ? "\<c-p>" : "\<s-tab>"
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
