#!/usr/bin/env python

import os
import sys
from optparse import OptionParser

__version__ = "0.0.1"

def arg_parse():
    """ Parse command line arguments """
    arg_list = "-m module [-o OUTPUT_DIRFILE]"
    usage = "Usage: %prog " + arg_list
    parser = OptionParser(usage, version=__version__)

    parser.add_option("-o", "--output_dir", dest="outputdir",
                      help="output directory",
                      default='.')

    parser.add_option("-m", "--module", dest="module",
                      help="module",
                      action="append",
                      default=None)

    parser.add_option("-f", "--force", dest="force",
                      help="overwrite files",
                      action="store_true",
                      default=False)

    return parser.parse_args()[0]

def main(options):

    move = ['north', 'west', 'south', 'east', 'first', 'random']
    action = ['random']
    zoom = ['less', 'more', 'random']

    skeleton = """
<!--
Module : {0}
Action : {1}
 -->
<setdynvars sourcetype="erlang" callback="{0}:{1}">
  <var name="list_url" />
</setdynvars>

<foreach name="element" in="list_url">
  <request subst="true">
    <http url="/%%_element%%.png" method="GET" version="1.1"/>
  </request>
</foreach>
"""
    entity = '<!ENTITY {0}_{1}_{2} SYSTEM "{0}_{1}_{2}.xml">'
    ents = []
    for module in options.module:
        for elmt in move:
            render(options, module, 'move', elmt, skeleton)
            ents.append(entity.format(module, 'move', elmt))
        for elmt in zoom:
            render(options, module, 'zoom', elmt, skeleton)
            ents.append(entity.format(module, 'zoom', elmt))
        for elmt in action:
            render(options, module, 'action', elmt, skeleton)
            ents.append(entity.format(module, 'action', elmt))

    mainfile(options, ents)

def mainfile(options, ents):
    """Create or update the main file
    """
    fpath = os.path.join(options.outputdir, "tsung.xml")

    if not os.path.exists(fpath) or options.force:
        print "write {}".format(fpath)
        write_file(fpath,
                   xmlheader().format('\n'.join(ents)))
    else:
        print "{} exists, add -f to overwrite it".format(fpath)

def write_file(fpath, datas):
    """Write a file with datas
    """
    f = open(fpath, 'w')
    f.write(datas)
    f.close()

def xmlheader():
    """xml header use to generate the main file
    
    Return : (string)
    """
    return """<?xml version="1.0"?>
<!DOCTYPE tsung SYSTEM "/usr/share/tsung/tsung-1.0.dtd"
[
{}
]
>
<tsung loglevel="notice" version="1.0">
  <!-- Client side setup -->
  <clients>
    <client host="localhost" use_controller_vm="true"/>
  </clients>

  <!-- Server side setup -->
  <servers>
    <server host="myserver" port="80" type="tcp"></server>
  </servers>

  &options;
  &load;

  <sessions>
    <session name="init">

    </session>
  </sessions>

</tsung>"""

def render(options, module, action, subaction, skeleton):
    """Write an entity file
    """
    name = '{}_{}_{}.xml'.format(module, action, subaction)
    fpath = os.path.join(options.outputdir, name)
    print "write {}".format(fpath)
    write_file(fpath, skeleton.format(module, name))

if __name__ == '__main__':
    opts = arg_parse()

    if not os.path.isdir(opts.outputdir):
        print '{} directory does not exists'.format(opts.outputdir)
        sys.exit(1)

    if opts.module is not None:
        main(opts)
    else:
        print "please add at least one module with -m"
        sys.exit(1)
