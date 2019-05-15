set hidden
set noshowmode

" don't give |ins-completion-menu| messages.
set shortmess+=c
" Some servers have issues with backup files, see #649
set nobackup
set nowritebackup

" Better display for messages
set cmdheight=2
" Smaller updatetime for CursorHold & CursorHoldI
set updatetime=300
" always show signcolumns
set signcolumn=yes
" Remove '-- INSERT --' line since it is shown in lighline anyway
set clipboard=unnamedplus


" Reload .vimrc on save
autocmd! bufwritepost .vimrc source %

" Disable arrows
let g:elite_mode=1

" Enable hard time using hjkl
"let g:hardtime_default_on = 1


" Autoinstall Plug if not available
if empty(glob('~/.vim/autoload/plug.vim'))
  silent execute "!curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif
" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')
" Disable search highligh when done searching
Plug 'romainl/vim-cool'

" Additional targets
Plug 'wellle/targets.vim'

" Python docstrings
Plug '~/.vim/plugged/python-gendoc'

" Python semantic highlighting
Plug 'numirias/semshi', {'do': ':UpdateRemotePlugins'}

" Show function signatures in commandline
Plug 'Shougo/echodoc.vim'

" Vim session handling made easy
Plug 'thaerkh/vim-workspace'

" Python/Braceless language text objects
" Plug 'tweekmonster/braceless.vim'

" Plug 'Yggdroot/indentLine'

"" Autocomplete framework
" Deoplete
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif
Plug 'davidhalter/jedi-vim'
Plug 'zchee/deoplete-jedi'

" Plug 'neoclide/coc.nvim', {'do': 'yarn install --frozen-lockfile'}

" Markdown support
" Plug 'tpope/vim-markdown'

Plug 'ervandew/supertab'

" Gruvbox colorscheme
Plug 'morhetz/gruvbox'

" Enable multiple cursors with <C-n> in visual mode
Plug 'terryma/vim-multiple-cursors'

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
" Plug 'plytophogy/vim-virtualenv'

" FZF
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Sensible vim config
Plug 'tpope/vim-sensible'

" Bash support
Plug 'vim-scripts/bash-support.vim'

" CTag automation
Plug 'ludovicchabant/vim-gutentags'

" Bracket autocomplete
Plug 'jiangmiao/auto-pairs'

" Advanced syntax highlighting
Plug 'sheerun/vim-polyglot'

" Statusline
Plug 'itchyny/lightline.vim'

" Julia support
Plug 'JuliaEditorSupport/julia-vim'

" Hex color preview
Plug 'lilydjwg/colorizer'

" Language spell/grammer checker, invoke with: :LanguageToolCheck
" Plug 'dpelle/vim-LanguageTool'

" UtilSnips
" Track the engine.
Plug 'SirVer/ultisnips'

" " Snippets are separated from the engine. Add this if you want them:
Plug 'honza/vim-snippets'

" Add repeat support for plugins
Plug 'tpope/vim-repeat'
Plug 'godlygeek/tabular'

" Initialize plugin system
call plug#end()

filetype plugin indent on    " required

"Credit joshdick
"Use 24-bit (true-color) mode in Vim/Neovim when outside tmux.
"If you're using tmux version 2.2 or later, you can remove the outermost $TMUX check and use tmux's 24-bit color support
"(see < http://sunaku.github.io/tmux-24bit-color.html#usage > for more information.)
if (empty($TMUX))
  "For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
  "Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
  " < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
  if (has("termguicolors"))
    set termguicolors
  endif
endif

" General vim config {{{
" Set leader key to <space> 
:let mapleader = ' '      " Leader key
:let maplocalleader = ',' " Local leader key

syntax on                 " Enable syntax highlighting
let g:gruvbox_italic=1
let g:gruvbox_bold=1
" let g:gruvbox_contrast_dark='soft'
set background=dark
colorscheme gruvbox
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

" Map Ctrl-c to esc
" imap <C-c> <Esc>

"split navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Custom mappings
nnoremap H ^
nnoremap L $

" Keep visual mode while indenting
vnoremap < <gv
vnoremap > >gv
" }}}


" Lightline {{{
let g:lightline = {
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ], [ 'gitbranch', 'readonly', 'relativepath', 'modified' ] ],
      \   'right' : [ ['lineinfo'], ['percent'], ['filetype'] ]
      \ },
      \ 'colorscheme': 'gruvbox',
      \ 'component_function': {
      \   'gitbranch' : 'fugitive#head',
      \ }
      \ }



let g:lightline.separator = {
      \   'left': '', 'right': ''
      \}
let g:lightline.subseparator = {
      \   'left': '', 'right': '' 
      \}

" }}}

" FZF {{{
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
" }}}


" Disable arrow movement, resize splits instead.
if get(g:, 'elite_mode')
  nnoremap <Up>    :resize +2<CR>
  nnoremap <Down>  :resize -2<CR>
  nnoremap <Left>  :vertical resize +2<CR>
  nnoremap <Right> :vertical resize -2<CR>
endif


" Jedi {{{
let g:jedi#rename_command = "<leader>r"
let g:jedi#auto_close_doc = 1
let g:jedi#usages_command = '<Leader>u'
let g:jedi#goto_command = "gd"
" Disable since deoplete is enabled
let g:jedi#auto_initialization = 1
let g:jedi#completions_enabled = 0
let g:jedi#auto_vim_configuration = 0
let g:jedi#smart_auto_mappings = 0
let g:jedi#popup_on_dot = 0
let g:jedi#completions_command = ""
let g:jedi#show_call_signatures = "2"
let g:jedi#show_call_signatures_modes = 'ni'  " ni = also in normal mode
" }}}

" Deoplete {{{
set pumheight=12
let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_ignore_case = 1
let g:deoplete#enable_smart_case = 1
" Disable autocompletion (using deoplete instead)
let g:jedi#completions_enabled = 0
" set completeopt-=preview
let g:python_host_prog = '/usr/bin/python'
let g:python3_host_prog = '/usr/bin/python3'
" let g:deoplete#auto_complete_delay = 100
" let g:deoplete#sources#jedi#show_docstring=1
" }}}


" EasyMotion {{{
let g:EasyMotion_do_mapping = 0 " Disable default mappings
let g:EasyMotion_smartcase = 1
" }}}

" Customize fzf colors to match your color scheme
autocmd! FileType fzf
autocmd  FileType fzf set laststatus=0 noshowmode noruler
      \| autocmd BufLeave <buffer> set laststatus=2 noshowmode ruler

" Disable trailing spaces warning
let g:python_highlight_space_errors=0

" ALE config {{{
" Ale fixers
let g:ale_fixers = ['black'] 
let g:ale_fix_on_save = 1

" Highlights
let g:ale_set_highlights = 0

" Check Python files with flake8 and pylint.
let b:ale_linters = ['flake8']
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
" Lint always in Normal Mode
let g:ale_lint_on_text_changed = 'normal'
" Lint when leaving Insert Mode but don't lint when in Insert Mode 
let g:ale_lint_on_insert_leave = 1
" }}}


" LaTeX {{{
" Disable latex-box from polyglot dependency to make vimtex usable
let g:polyglot_disabled = ['latex', 'markdown']
" let g:polyglot_disabled = ['markdown']

let g:tex_flavor='latex'
let g:vimtex_view_method='zathura'
let g:vimtex_quickfix_mode=0
set conceallevel=1
let g:tex_conceal='abdmg'


" LaTeX bindings {{{
augroup latexbindings
  autocmd! latexbindings
  autocmd Filetype tex inoremap <buffer> <silent> _ _{}<Left>

  autocmd BufWritePost *.tex :silent execute ':!pdflatex homework.tex'
augroup end
" }}}
" }}}

" UltiSnips {{{
let g:UltiSnipsExpandTrigger = '<tab>'
let g:UltiSnipsJumpForwardTrigger = '<c-b>'
let g:UltiSnipsJumpBackwardTrigger = '<c-z>'
let g:UltiSnipsSnippetDirectories=["UltiSnips"]
" }}}


" Disable gitgutter mappings
let g:gitgutter_map_keys = 0



" Python bindings {{{
augroup pythonbindings
  autocmd! pythonbindings
  " Refactor with ALE black
  autocmd Filetype python nnoremap <buffer> <silent> <localleader>r :ALEFix black<CR>

  " Insert debugging with ipdb
  autocmd Filetype python nnoremap <buffer> <silent> <localleader>b :call InsertIPDB()<CR>

  " Python import
  autocmd Filetype python nnoremap <buffer> <silent> <localleader>i     :ImportName<CR>

  " autocmd Filetype python vnoremap <buffer> <silent> <localleader>d :'<,'>GenPyDoc<CR>
  " Function to insert python IPDB debug line
  function! InsertIPDB()
    let trace = expand("import ipdb; ipdb.set_trace(context=9)")
    execute "normal O".trace
  endfunction

  " 
  " autocmd BufWritePre *.py execute ':ALEFix black'
augroup end
" }}}

" LanguageTool config {{{
let g:languagetool_jar = '$HOME/Downloads/LanguageTool-4.3/languagetool-commandline.jar'
hi LanguageToolGrammarError  guisp=#b58900 gui=undercurl guifg=NONE guibg=NONE ctermfg=white ctermbg=blue term=underline cterm=none
hi LanguageToolSpellingError guisp=#dc322f  gui=undercurl guifg=NONE guibg=NONE ctermfg=white ctermbg=red  term=underline cterm=none
" }}}

" UtilSnips {{{
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"
let g:ultisnips_python_style="google"
" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"
" }}}

" Supertab {{{
let g:SuperTabDefaultCompletionType = "<c-n>"
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
nnoremap <silent> <Leader>b :Buffers<CR>
nnoremap <silent> <Leader>l :Lines<CR>
nnoremap <silent> <Leader>t :BTags<CR>
nnoremap <silent> <Leader>T :Tags<CR>
nnoremap <silent> <Leader>f :Files<CR>
nnoremap <silent> <Leader>h :History:<CR>
nnoremap <silent> <Leader>/ :Ag<CR>
nnoremap <silent> <Leader><Tab> :b#<CR>

" Move to word
map  <Leader>w <Plug>(easymotion-bd-w)
" " Move to line
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
" }}


" Vim workspace {{{
nnoremap <leader>s :ToggleWorkspace<CR>
let g:workspace_session_directory = $HOME . '/.vim/sessions/'
let g:workspace_autosave = 0
let g:workspace_autosave_untrailspaces = 0
" }}}

" Vim markdown{{{
let g:vim_markdown_folding_disabled = 1
" }}}

" Unmap malicious plugin bindings
let g:colorizer_nomap = 1


" Python-gendoc
let g:python_gendoc_style = 'google'

" Vim cool{{{
let g:CoolTotalMatches = 1
" }}}

" CoC Vim {{{
" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
" Use `:Format` to format current buffer
" command! -nargs=0 Format :call CocAction('format')

" inoremap <silent><expr> <TAB>
"       \ pumvisible() ? "\<C-n>" :
"       \ <SID>check_back_space() ? "\<TAB>" :
"       \ coc#refresh()
" inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

" function! s:check_back_space() abort
"   let col = col('.') - 1
"   return !col || getline('.')[col - 1]  =~# '\s'
" endfunction

" " Use <c-space> to trigger completion.
" inoremap <silent><expr> <c-space> coc#refresh()

" " Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
" " Coc only does snippet and additional edit on confirm.
" inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm() : 
"                                            \"\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"


" " Use K to show documentation in preview window
" nnoremap <silent> K :call <SID>show_documentation()<CR>

" function! s:show_documentation()
"   if (index(['vim','help'], &filetype) >= 0)
"     execute 'h '.expand('<cword>')
"   else
"     call CocAction('doHover')
"   endif
" endfunction

" " Remap keys for gotos
" nmap <silent> gd <Plug>(coc-definition)
" nmap <silent> gy <Plug>(coc-type-definition)
" nmap <silent> gi <Plug>(coc-implementation)
" nmap <silent> gr <Plug>(coc-references)


" " Highlight symbol under cursor on CursorHold
" " autocmd CursorHold * silent call CocActionAsync('highlight')

" " Remap for rename current word
" nmap <leader>rn <Plug>(coc-rename)
" }}}


" Custom semshi Highlights {{{
function! CustomSemshiHighlights()
  hi semshiLocal           ctermfg=208 guifg=#fe8019
  hi semshiGlobal          ctermfg=172 guifg=#d79921
  hi semshiImported        ctermfg=172 guifg=#d79921 cterm=bold gui=bold
  hi semshiParameter       ctermfg=109 guifg=#83a598
  hi semshiParameterUnused ctermfg=108 guifg=#7c6f64 cterm=underline gui=underline
  hi semshiFree            ctermfg=176 guifg=#d3869b
  hi semshiBuiltin         ctermfg=132 guifg=#b16286
  hi semshiAttribute       ctermfg=108 guifg=#8ec07c
  hi semshiSelf            ctermfg=248 guifg=#bdae93
  hi semshiUnresolved      ctermfg=166 guifg=#d65d0e cterm=underline gui=underline
  hi semshiSelected        ctermfg=230 guifg=#f9f5d7 ctermbg=237 guibg=#504945

  hi semshiErrorSign       ctermfg=230 guifg=#f9f5d7 ctermbg=124 guibg=#cc241d
  hi semshiErrorChar       ctermfg=230 guifg=#f9f5d7 ctermbg=124 guibg=#cc241d
  sign define semshiError text=E> texthl=semshiErrorSign
endfunction
autocmd filetype python call CustomSemshiHighlights()
" }}}


" GRUVBOX color table https://github.com/morhetz/gruvbox-contrib/blob/master/color.table
" GRUVCOLR         HEX       RELATV ALIAS   TERMCOLOR      RGB           ITERM RGB     OSX HEX
"  --------------   -------   ------------   ------------   -----------   -----------   -------
"  dark0_hard       #1d2021   [   ]  [   ]   234 [h0][  ]    29- 32- 33    22- 24- 25   #161819
"  dark0            #282828   [bg0]  [fg0]   235 [ 0][  ]    40- 40- 40    30- 30- 30   #1e1e1e
"  dark0_soft       #32302f   [   ]  [   ]   236 [s0][  ]    50- 48- 47    38- 36- 35   #262423
"  dark1            #3c3836   [bg1]  [fg1]   237 [  ][15]    60- 56- 54    46- 42- 41   #2e2a29
"  dark2            #504945   [bg2]  [fg2]   239 [  ][  ]    80- 73- 69    63- 57- 53   #3f3935
"  dark3            #665c54   [bg3]  [fg3]   241 [  ][  ]   102- 92- 84    83- 74- 66   #534a42
"  dark4            #7c6f64   [bg4]  [fg4]   243 [  ][ 7]   124-111-100   104- 92- 81   #685c51

"  gray_245         #928374   [gray] [   ]   245 [ 8][  ]   146-131-116   127-112- 97   #7f7061
"  gray_244         #928374   [   ] [gray]   244 [  ][ 8]   146-131-116   127-112- 97   #7f7061

"  light0_hard      #f9f5d7   [   ]  [   ]   230 [  ][h0]   249-245-215   248-244-205   #f8f4cd
"  light0           #fbf1c7   [fg0]  [bg0]   229 [  ][ 0]   251-241-199   250-238-187   #faeebb
"  light0_soft      #f2e5bc   [   ]  [   ]   228 [  ][s0]   242-229-188   239-223-174   #efdfae
"  light1           #ebdbb2   [fg1]  [bg1]   223 [15][  ]   235-219-178   230-212-163   #e6d4a3
"  light2           #d5c4a1   [fg2]  [bg2]   250 [  ][  ]   213-196-161   203-184-144   #cbb890
"  light3           #bdae93   [fg3]  [bg3]   248 [  ][  ]   189-174-147   175-159-129   #af9f81
"  light4           #a89984   [fg4]  [bg4]   246 [ 7][  ]   168-153-132   151-135-113   #978771

"  bright_red       #fb4934   [red]   [  ]   167 [ 9][  ]   251- 73- 52   247- 48- 40   #f73028
"  bright_green     #b8bb26   [green] [  ]   142 [10][  ]   184-187- 38   170-176- 30   #aab01e
"  bright_yellow    #fabd2f   [yellow][  ]   214 [11][  ]   250-189- 47   247-177- 37   #f7b125
"  bright_blue      #83a598   [blue]  [  ]   109 [12][  ]   131-165-152   113-149-134   #719586
"  bright_purple    #d3869b   [purple][  ]   175 [13][  ]   211-134-155   199-112-137   #c77089
"  bright_aqua      #8ec07c   [aqua]  [  ]   108 [14][  ]   142-192-124   125-182-105   #7db669
"  bright_orange    #fe8019   [orange][  ]   208 [  ][  ]   254-128- 25   251-106- 22   #fb6a16

"  neutral_red      #cc241d   [   ]  [   ]   124 [ 1][ 1]   204- 36- 29   190- 15- 23   #be0f17
"  neutral_green    #98971a   [   ]  [   ]   106 [ 2][ 2]   152-151- 26   134-135- 21   #868715
"  neutral_yellow   #d79921   [   ]  [   ]   172 [ 3][ 3]   215-153- 33   204-136- 26   #cc881a
"  neutral_blue     #458588   [   ]  [   ]    66 [ 4][ 4]    69-133-136    55-115-117   #377375
"  neutral_purple   #b16286   [   ]  [   ]   132 [ 5][ 5]   177- 98-134   160- 75-115   #a04b73
"  neutral_aqua     #689d6a   [   ]  [   ]    72 [ 6][ 6]   104-157-106    87-142- 87   #578e57
"  neutral_orange   #d65d0e   [   ]  [   ]   166 [  ][  ]   214- 93- 14   202- 72- 14   #ca480e

"  faded_red        #9d0006   [   ]   [red]   88 [  ][ 9]   157-  0-  6   137-  0-  9   #890009
"  faded_green      #79740e   [   ] [green]  100 [  ][10]   121-116- 14   102- 98- 13   #66620d
"  faded_yellow     #b57614   [   ][yellow]  136 [  ][11]   181-118- 20   165- 99- 17   #a56311
"  faded_blue       #076678   [   ]  [blue]   24 [  ][12]     7-102-120    14- 83-101   #0e5365
"  faded_purple     #8f3f71   [   ][purple]   96 [  ][13]   143- 63-113   123- 43- 94   #7b2b5e
"  faded_aqua       #427b58   [   ]  [aqua]   66 [  ][14]    66-123- 88    53-106- 70   #356a46
"  faded_orange     #af3a03   [   ][orange]  130 [  ][  ]   175- 58-  3   157- 40-  7   #9d2807

