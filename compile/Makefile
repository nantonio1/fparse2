fc = gfortran
root = ..
build = $(root)/build
src = $(root)/src
examples = $(root)/examples

objs = \
    $(build)/fparse_types.o \
    $(build)/fparse_util.o \
    $(build)/fparse_schema.o \
    $(build)/fparse_io.o \
    $(build)/fparse_parser.o \
    $(build)/fparse_getters.o \
    $(build)/fparse.o

all: $(build)/example_main

$(build):
	mkdir -p $(build)

$(build)/%.o: $(src)/%.f90 | $(build)
	$(fc) -c $< -J $(build) -I $(build) -o $@

$(build)/example_main: $(objs) $(examples)/example_main.f90 | $(build)
	$(fc) $(objs) $(examples)/example_main.f90 \
	-J $(build) -I $(build) -o $@

clean:
	rm -f $(build)/*.o $(build)/*.mod $(build)/example_main
