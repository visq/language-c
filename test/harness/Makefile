TESTS=analysis_enum \
      analysis_ext_decls \
      analysis_local_decls \
      analysis_type_check attributes \
      builtins \
      bug5_dos_newline bugn6_empty_file bug21_sem_typedef \
      bug22_file_permission_cpp bug30_preserve_int_repr bug31_pp_if_else \
      bug20130805_nopos \
      bug20160302_int128 \
      bug20160314_noreturn \
      bug20160729_C_include_stack \
      bug20160911_builtin_bswap \
      parse_dg

#bug20140111_utf8
all: tests
tests: $(TESTS:=.runtest)
clean: $(TESTS:=.runclean)
%.runtest:
	make -C $* clean all
%.runclean:
	make -C $* clean

