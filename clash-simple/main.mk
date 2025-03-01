
TABLES_DIR = ../tables
GEN_GAMMA = $(TABLES_DIR)/gen_gamma_table

all: $(PROJ).rpt $(PROJ).bin

top.v: src/Example/Project.hs
	cabal run clash -- $^ --verilog

%.blif: $(PROJ_DIR)/%.v $(ADD_SRC)
	yosys -ql $*.log -p 'synth_ice40 -top top -blif $@' $< $(ADD_SRC)

%.json: $(PROJ_DIR)/%.v $(ADD_SRC)
	yosys -ql $*.log -p 'synth_ice40 -top top -json $@' $< $(ADD_SRC)

ifeq ($(USE_ARACHNEPNR),)
%.asc: $(PIN_DEF) %.json
	nextpnr-ice40 -q --$(DEVICE) --json $(filter-out $<,$^) --pcf $< --asc $@
else
%.asc: $(PIN_DEF) %.blif
	arachne-pnr -d $(subst up,,$(subst hx,,$(subst lp,,$(DEVICE)))) -o $@ -p $^
endif

%.bin: %.asc
	icepack $< $@

%.rpt: %.asc
	icetime -d $(DEVICE) -mtr $@ $<

prog: $(PROJ).bin
	iceprog $<

sudo-prog: $(PROJ).bin
	@echo 'Executing prog as root!!!'
	sudo iceprog $<

clean:
	rm -f $(PROJ).blif $(PROJ).asc $(PROJ).log $(PROJ).rpt $(PROJ).bin \
	      $(PROJ).json ../tables/gen_gamma_table *.vcd a.out $(ADD_CLEAN)

%.hex:	../tables/gen_gamma_table

$(GEN_GAMMA): $(TABLES_DIR)/gen_gamma_table.c
$(GEN_GAMMA): LDLIBS += -lm

.SECONDARY:
.PHONY: all prog clean
