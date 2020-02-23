"set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')
"
let g:ycm_confirm_extra_conf = 0 

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'timonv/vim-cargo'

" youCompleteMe for code completion. run installation process after Plugin is
" pulled in
Plugin 'Valloric/YouCompleteMe'
Plugin 'rdnetto/YCM-Generator'
Plugin 'dbeniamine/cheat.sh-vim'

" YCM configurations and kwymaps
filetype plugin indent on
map <C-]> :YcmCompleter GoToImprecise<CR>
nnoremap <F5> :YcmForceCompileAndDiagnostics<CR>
nnoremap <leader>jd :YcmCompleter GoTo<CR>
let g:ycm_always_populate_location_list = 1
let g:ycm_python_binary_path = 'python'

Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
"
" air-line
set laststatus=2
let g:airline_theme='atomic'
set guifont=Meslo\ LG\ S\ Regular\ for\ Powerline
let g:airline_powerline_fonts = 1
let g:airline_section_b = '%{strftime("%T")}'
let g:airline_section_y = ''
set noshowmode
set t_Co=256

Plugin 'flazz/vim-colorschemes'
Plugin 'vim-latex/vim-latex'
Plugin 'pangloss/vim-javascript'

"Git wrapper for Vim
Plugin 'tpope/vim-fugitive'

" Nerdtree
Plugin 'scrooloose/nerdtree'
noremap \\ :NERDTreeToggle<CR>
noremap \f :NERDTreeFind<CR>
Plugin 'sjl/tslime.vim'
" tslime {{{
let g:tslime_ensure_trailing_newlines = 1
let g:tslime_normal_mapping = '<localleader>t'
let g:tslime_visual_mapping = '<localleader>t'
let g:tslime_vars_mapping = '<localleader>T'
" " }}}
Plugin 'amdt/vim-niji'
let g:niji_dark_colours = [
    \ [ '81', '#5fd7ff'],
    \ [ '99', '#875fff'],
    \ [ '1',  '#dc322f'],
    \ [ '76', '#5fd700'],
    \ [ '3',  '#b58900'],
    \ [ '2',  '#859900'],
    \ [ '6',  '#2aa198'],
    \ [ '4',  '#268bd2'],
    \ ]
Plugin 'vim-scripts/paredit.vim'
Plugin 'christoomey/vim-tmux-navigator'
" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

set autowrite
set showcmd
set ruler
set nowrap
set autoindent
set smartindent
set shiftround
set ignorecase
set smartcase
set smarttab
set hlsearch  "highlight search terms
set incsearch "show search matches as you type
set title
set backspace=indent,eol,start

nnoremap ; :
nnoremap j gj
nnoremap k gk
nmap <silent> ,/ :nohlsearch<CR>
nnoremap Y y$

set grepprg=grep\ -nH\ $*
set pastetoggle=<F2>

let g:Tex_DefaultTargetFormat='pdf'
let g:Tex_CompileRule_pdf ='xelatex -file-line-error -interaction=nonstopmode $*'
let g:Tex_ShowErrorContext = 0
let g:Tex_GotoError=0
let Tex_FoldedSections=""
let Tex_FoldedEnvironments=""
let Tex_FoldedMisc=""
set shellslash
set grepprg=grep\ -nH\ $*
let g:tex_flavor='latex'
if has('unix')
  if has('mac')
    let g:Tex_TreatMacViewerAsUNIX = 1
    let g:Tex_ViewRule_pdf = 'open -a Preview'
  else
    let g:Tex_ViewRule_pdf = 'evince 2>/dev/null'
  endif
endif
set number
set numberwidth=5
let mysyntaxfile = "~/.vim/mysyntax.vim"
syntax on
set tabstop=2 softtabstop=0 expandtab shiftwidth=2 smarttab
set ai
set showcmd
set background=dark
autocmd ColorScheme janah highlight Normal ctermbg=235
colorscheme janah
highlight Normal ctermbg=NONE
highlight nonText ctermbg=NONE
set smartcase
"set relativenumber
"Youcompleteme fix

"fugitive git bindings
nnoremap <space>gc :Gcommit -a<CR>
nnoremap <space>ga :Git add %:p<CR><CR>
nnoremap <space>gd :Gdiff<CR>
nnoremap <space>gp :Gpush<CR>
nnoremap fi gg=G
nnoremap <tab> %
vnoremap <tab> %


let $PATH = '/usr/local/bin:'.$PATH
set splitbelow
set splitright

let g:C_Ctrl_j = 'off'
let g:BASH_Ctrl_j = 'off'
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

" air-line

if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

map <Leader>lx :<C-U>call CompileXeTex()<CR>
hi Normal ctermbg=none
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.whitespace = 'Ξ'

" airline symbols
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''

autocmd filetype lisp,scheme,art setlocal equalprg=scmindent.rkt
