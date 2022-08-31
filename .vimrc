" ============ "
" ===Basics=== "
" ============ "
syntax on
set noerrorbells 
set belloff=all
set expandtab 
set nu
set nowrap
set smartcase
set noswapfile
set nobackup
set incsearch
set autoindent
set laststatus=2
set encoding=UTF-8
set t_Co=256 
set splitbelow
cabbrev bterm bo term

" File-types
autocmd BufNewFile,BufRead *.go set filetype=go

" Tabs
set sw=2 ts=2 sts=2 " Default
autocmd FileType html :setlocal sw=2 ts=2 sts=2
autocmd FileType ruby :setlocal sw=2 ts=2 sts=2
autocmd FileType javascript :setlocal sw=2 ts=2 sts=2
autocmd FileType xml :setlocal sw=2 ts=2 sts=2
autocmd FileType python :setlocal sw=4 ts=4 sts=4
autocmd FileType go :setlocal sw=4 ts=4 sts=4

" Make vertical separator pretty
highlight VertSplit cterm=NONE
set fillchars+=vert:\▏

" ============== "
" ===Mappings=== "
" ============== "
let mapleader = " "

" Pane navigation
noremap <C-J> <C-W><C-J>
noremap <C-K> <C-W><C-K>
noremap <C-L> <C-W><C-L>
noremap <C-H> <C-W><C-H>

" For vim-terminal
tnoremap <C-J> <C-W><C-J>
tnoremap <C-K> <C-W><C-K>
tnoremap <C-L> <C-W><C-L>
tnoremap <C-H> <C-W><C-H>
tnoremap <C-b> <C-\><C-n>


noremap <leader>c "*yy<cr>
noremap<leader>v "+p<cr>
noremap <leader>p :Files<cr>
noremap <leader>g :GFiles<cr>
noremap <leader>b :Buffers<cr>
noremap <leader>e :NERDTreeToggle<cr>
noremap <leader>/ :BTerm<cr>
noremap <leader>= <C-W><C-=>
noremap <leader>n gt
noremap <leader>N gT
noremap <leader>t :tabnew <cr>
noremap <leader>s :vsplit <cr>



" This is auto complete fix after COC did the update 
" the broke my config!

function! CheckBackspace() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction


inoremap <silent><expr> <TAB>
      \ coc#pum#visible() ? coc#pum#next(1) :
      \ CheckBackspace() ? "\<Tab>" :
      \ coc#refresh()
inoremap <expr><S-TAB> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"

" Make <CR> to accept selected completion item or notify coc.nvim to format
" <C-g>u breaks current undo, please make your own choice.
inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" ============="
" ===Plugins==="
" ============="
call plug#begin('~/.vim/plugged')
    Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
    Plug 'junegunn/fzf.vim'
    Plug 'ggreer/the_silver_searcher'
    Plug 'preservim/nerdtree'
    Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
    Plug 'tpope/vim-fugitive'
    Plug 'preservim/nerdcommenter'
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'
    Plug 'ryanoasis/vim-devicons'
    Plug 'vim-syntastic/syntastic'

    " colorschemes
    Plug 'sainnhe/everforest'
    Plug 'ayu-theme/ayu-vim'
    Plug 'sainnhe/gruvbox-material'

    " COC server "
    Plug 'neoclide/coc.nvim', {'branch': 'release'}

    " TypeScript
    Plug 'leafgarland/typescript-vim'
    Plug 'peitalin/vim-jsx-typescript'

    " JavaScript
    Plug 'pangloss/vim-javascript'
    Plug 'maxmellon/vim-jsx-pretty'
    Plug 'styled-components/vim-styled-components' 

    " GLSL
    Plug 'tikhomirov/vim-glsl'
call plug#end()

" Current color scheme "


" Important!!
        if has('termguicolors')
          set termguicolors
        endif
        " For dark version.
        set background=dark
        " This configuration option should be placed before `colorscheme everforest`.
        " Available values: 'hard', 'medium'(default), 'soft'
        let g:everforest_background = 'hard'
        " For better performance
        let g:everforest_better_performance = 1
        colorscheme gruvbox-material

let g:airline_theme='base16'

" Must come after colorscheme command
" Ensure the any colorscheme has transparent bg
hi Normal guibg=NONE ctermbg=NONE

" ============="
" PluginConfigs"
" ============="

" nerdtree
"let NERDTreeShowHidden = 1
let NERDTreeMinimalUI=1
autocmd BufEnter * if tabpagenr('$') == 1 
      \ && winnr('$') == 1 
      \ && exists('b:NERDTree') 
      \ && b:NERDTree.isTabTree()
      \ | quit | endif

" vim-nerdtree-syntax-highlight
let g:NERDTreeHighlightFolders = 1
let g:NERDTreeHighlightFoldersFullName = 1


let g:NERDTreeSyntaxEnabledExtensions = ['rb', 'ruby']

" vim-devicons
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols = {}
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols['html'] = ''
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols['js'] = ''
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols['json'] = ''
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols['jsx'] = 'ﰆ'
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols['md'] = ''
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols['vim'] = ''
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols['yaml'] = ''
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols['yml'] = ''

let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols = {}
let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols['.*vimrc.*'] = ''
let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols['.gitignore'] = ''
let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols['package.json'] = ''
let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols['package.lock.json'] = ''
let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols['node_modules'] = ''
let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols['webpack\.'] = 'ﰩ'
let g:DevIconsEnableFoldersOpenClose = 1

" vim-airlines
let g:airline_powerline_fonts = 1
let g:airline#extensions#bufferline#enabled = 1
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''

" fzf.vim
let g:fzf_colors= {
      \  'border': ['fg', 'Type' ],
      \  'gutter': ['fg', 'Type' ] }

" vim-jsx-pretty
hi jsxAttrib ctermfg=3*
hi jsxComponentName ctermfg=4*
hi jsxTagName ctermfg=4*
hi jsxPunct ctermfg=3*
hi jsObjectProp ctermfg=3*
hi jsxCloseString ctermfg=3*

" typescript-vim
let g:typescript_indent_disable = 1
hi javaScriptLineComment ctermfg=4*

" vim-go
let g:go_highlight_structs = 1 
let g:go_highlight_methods = 1
let g:go_highlight_functions = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
let g:go_highlight_variable_declarations = 1
let g:go_highlight_format_strings = 1
let g:go_highlight_string_spellcheck = 1
let g:go_highlight_types = 1
let g:go_highlight_function_calls = 1

" ============="
" ==CustomCmds="
" ============="

" Absolute path of open file to clipboard
function! Cwf()
    let @+=expand('%:p')
endfunction
command! Cwf call Cwf()

" Print working file_path
function! Pfp()
    echo expand('%')
endfunction
command! Pfp call Pfp()

" Relative path of open file to clipboard
function! Cwfr()
    let @+=expand('%')
endfunction
command! Cwfr call Cwfr()

" ex) :Tag h1
function! Tag(name)
    let @"="<" . a:name . "></" . a:name . ">"
    normal! pbbl
    startinsert
endfunction
command! -nargs=1 T call Tag(<f-args>)

" ex) :Jtag HelloWorld
function! Jtag(name)
    let @"="<" . a:name . " />"
    normal! pb
    startinsert
endfunction
command! -nargs=1 J call Jtag(<f-args>)

" Open up nerdtree and a bottom terminal
function In()
  execute "below term++rows=15"
  execute "NERDTreeToggle"
  execute "wincmd l"
endfunction
command! In call In()

" Open up a bottom terminal
function BTerm()
  execute "below term++rows=15"
endfunction
command! BTerm call BTerm()

function GColor()
  execute "echo synIDattr(synIDtrans(synID(line(\".\"), col(\".\"), 1)), \"fg#\")"
endfunction
command! GColor call GColor()

function F()
  execute "NERDTreeFind"
endfunction
command! F call F()

function LineUp()
  " There be a whitespace at the end of next line
  normal! ^dg_k$A 
  normal! pjdd
endfunction
command! LU call LineUp()

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction
