
SUBDIRS = jsx 
CLEANSUBDIRS = $(SUBDIRS:%=clean-%)

.PHONY: subdirs $(SUBDIRS) $(CLEANSUBDIRS)


all: subdirs
     
subdirs: $(SUBDIRS)
clean: $(CLEANSUBDIRS)
     
$(SUBDIRS):
	$(MAKE) -C $@

$(CLEANSUBDIRS): 
	$(MAKE) -C $(@:clean-%=%) clean
