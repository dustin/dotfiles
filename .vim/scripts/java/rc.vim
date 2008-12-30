" _jd helps me make javadoc
map _jd O/**<CR> */<ESC>O*

" Implement a set method
map _jsm mb0ww2yw`gO<CR><ESC>pyypki<TAB>private void set <ESC>wdwhx~A(<ESC>JxWito) {<CR><TAB>this.<ESC>A=to;<CR><ESC>0A<TAB>}<ESC>`b

" Implement a get method for a variable
map _jgm mb0ww2yw`gO<CR><ESC>pyypki<TAB>public <ESC>wwiget<ESC>b   ~A() {<CR>}<ESC>ddpkI<TAB><TAB><ESC> dwireturn(<ESC>A);<ESC>`b
" Make get and set methods for a variable
map <F12> _jsm_jgm

" Jcommenter for java files
" http://vim.sourceforge.net/script.php?script_id=20
let b:jcommenter_class_author='Dustin Sallings (dustin@spy.net)'
let b:jcommenter_file_author='Dustin Sallings (dustin@spy.net)'
source ~/.vim/scripts/java/jcommenter.vim
map <Leader>c :call JCommentWriter()<CR>

