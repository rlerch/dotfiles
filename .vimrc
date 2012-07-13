" Buck's vimrc
" ------------
"
" Installed plugins:
"
"    plugin name      short description
"    --------------------------------------------------------------------------
"   * NERDTree        Provides a way to peruse directories
"   * rename.vim      Allows renaming of files by doing :rename <file>
"   * a.vim           Easy switching between header and source files
"   * MRU             Remembers recently visited files to open quickly
"   * protodef        Creates skeleton C++ source files based on header files
"   * snipMate        Snippets!
"   * supertab        Allows the tab key to be fully used for omnicomplete
"   * surround        Surround text with tags, quotes, etc.
"   * tcomment        Quickly comment out lines or selections
"   * OmniCppComplete C++ Omni-Complete
"   * CtrlP           Fuzzy Finder

" basic settings --------------------------------------------------------------
set nocompatible " not vi-compatible
set rnu " shows relative line numbers
if has("gui_running")
    set background=dark
    colorscheme solarized
    set guioptions-=m  "remove menu bar
    set guioptions-=T  "remove toolbar
    set guioptions-=r  "remove right-hand scroll bar
    if has("gui_gtk2") " Running on Linux
        set guifont=Inconsolata\ 12
    elseif has("gui_win32") " Running on Windows
        set guifont=Inconsolata:h12:cANSI
        au GUIEnter * simalt ~x " starts gvim in maximized mode
    endif
endif
set bs=2 " needed on Windows for backspace to work properly

" tab settings ->
" RJMetrics uses tabs, not spaces
set autoindent " Uses indent from current line as indent for new line
set tabstop=4
" set expandtab " Expands tab into spaces
" set smarttab " Allows deleting of full tab at beginning of lines when it's turned into spaces
set shiftwidth=4 " # spaces to use for auto-indent
" set softtabstop=4
" <-

" Visual whitespace
set list
set listchars=tab:>\ ,trail:. 

set visualbell " Stops the 'ding' heard all the time

" searching
set incsearch " incremental search (i.e. search while typing)
set hlsearch  " highlight searched text
set ignorecase " ignore case on searches

set mouse=a " enables mouse use in all modes
syntax enable " enables syntax highlighting
filetype on " enables filetype detection
filetype plugin on

let mapleader = ","

" jump to last position on previous close
autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

" Brackets & paretheses
" bracket completion
inoremap {<CR> {<CR>}<Esc>O<Tab>

" other stuff -----------------------------------------------------------------
" From http://stackoverflow.com/questions/235439/vim-80-column-layout-concerns/235970#235970
highlight OverLength ctermbg=red ctermfg=white guibg=#DE7676
let g:OverLengthOn = 0
function OverLengthToggle()
    if g:OverLengthOn == 1
        match none
        let g:OverLengthOn = 0
    else
        match OverLength /.\%>81v/
        let g:OverLengthOn = 1
    endif
endfunction
nnoremap <C-h> :call OverLengthToggle()<CR>

" set cursorline
" highlight CursorLine guibg=#FFE0F7
" highlight CursorColumn guibg=#FFE0F7
nnoremap <Leader>l :set cursorline!<CR>
nnoremap <Leader>c :set cursorcolumn!<CR>

" general key mappings --------------------------------------------------------
" Change 'Y' to copy to end of line to be similar to D and C
nnoremap Y y$

" Fix mistake I often make -> typing :a instead of :wa
map :a<CR> :wa<CR>

" line movement mappings from http://vim.wikia.com/wiki/Moving_lines_up_or_down
" Use Alt-j or Alt-k to move lines up or down, respectively
nnoremap <A-j> :m+<CR>==
inoremap <A-j> <Esc>:m+<CR>==gi
vnoremap <A-j> :m'>+<CR>gv=gv
nnoremap <A-k> :m-2<CR>==
inoremap <A-k> <Esc>:m-2<CR>==gi
vnoremap <A-k> :m-2<CR>gv=gv

" Map Ctrl+Del in insert mode to delete back a word
inoremap <C-BS> <C-w>

" Proper Ctrl+C -> Esc map
imap <C-c> <Esc>
nmap <C-c> <Esc>

" fold mapping
nnoremap + zo
nnoremap - zc

" Tab movements
nnoremap <F5> :tabm<CR>
nmap <C-S-PageUp> :tabm tabpagenr()-1<CR>
nmap <C-S-PageDown> :tabm tabpagenr()+1<CR>

" Mapping to auto-format the entire document and return
" to original position
nnoremap <F8> mzgggqG`z

" automatically open and close the popup menu / preview window
" from: http://vim.wikia.com/wiki/C%2B%2B_code_completion
" TODO check for conflicts with supertab
au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
set completeopt=menu,menuone,longest,preview

" -----------------------------------------------------------------------------
" Plugin settings and mappings
" -----------------------------------------------------------------------------

" Tips for getting header/source switch came from
" http://vim.wikia.com/wiki/Easily_switch_between_source_and_header_file
" mappings for a.vim
nnoremap <F4> :AT<CR>
inoremap <F4> <Esc>:AT<CR>

" NERDTree settings and mappings
let NERDTreeIgnore=['\.swp$', '\.orig$', '\.pyc$', '\.class$']
" mapping to open NERDTree and close the split it creates
nnoremap <F3> :NERDTreeToggle<CR>
inoremap <F3> <Esc>:NERDTreeToggle<TR>a

" OmniCppComplete settings ---------------------------------------------------
let OmniCpp_MayCompleteDot = 1 " autocomplete after .
let OmniCpp_MayCompleteArrow = 1 " autocomplete after ->
let OmniCpp_MayCompleteScope = 1 " autocomplete after ::o

" Fuzzy Finder ---------------------------------------------------------------
let g:SuperTabDefaultCompletionType = "<C-x><C-o>"
let g:SuperTabLongestEnhanced=1 " Enhances 'longest' in 'completeopt' setting
let g:SuperTabLongestHighlight=1 " automatically highlights the first entry

" MiniBufExplorer ------------------------------------------------------------
let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapCTabSwitchBufs = 1
let g:miniBufExplUseSingleClick = 1

" XML Pretty formatting
function! DoPrettyXML()
  " save the filetype so we can restore it later
  let l:origft = &ft
  set ft=
  " delete the xml header if it exists. This will
  " permit us to surround the document with fake tags
  " without creating invalid xml.
  1s/<?xml .*?>//e
  " insert fake tags around the entire document.
  " This will permit us to pretty-format excerpts of
  " XML that may contain multiple top-level elements.
  0put ='<PrettyXML>'
  $put ='</PrettyXML>'
  silent %!xmllint --format -
  " xmllint will insert an <?xml?> header. it's easy enough to delete
  " if you don't want it.
  " delete the fake tags
  2d
  $d
  " restore the 'normal' indentation, which is one extra level
  " too deep due to the extra tags we wrapped around the document.
  silent %<
  " back to home
  1
  " restore the filetype
  exe "set ft=" . l:origft
endfunction
command! PrettyXML call DoPrettyXML()
