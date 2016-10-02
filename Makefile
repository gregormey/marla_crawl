PROJECT = marla_crawl
PROJECT_DESCRIPTION = A web crawler to grap adresses 
PROJECT_VERSION = 0.0.1

# Options.
CT_OPTS += -pa test -ct_hooks marla_crawl_ct_hook []

# Depandancies
DEPS = mnesia_utile
dep_mnesia_utile = git https://github.com/gregormey/mnesia_utile master

LOCAL_DEPS = mnesia inets

TEST_DEPS = ct_helper
dep_ct_helper = git https://github.com/extend/ct_helper.git master

include erlang.mk
