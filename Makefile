
SUBDIRS = haskell src java examples
CLEANSUBDIRS = $(SUBDIRS:%=clean-%)

.PHONY: subdirs $(SUBDIRS) $(CLEANSUBDIRS)


all: subdirs
     
subdirs: $(SUBDIRS)
clean: $(CLEANSUBDIRS)
	rm -f ebin/*
     
$(SUBDIRS):
	$(MAKE) -C $@

$(CLEANSUBDIRS): 
	$(MAKE) -C $(@:clean-%=%) clean
