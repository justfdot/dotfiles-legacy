# This file is part of ranger, the console file manager.
# License: GNU GPL version 3, see the file "AUTHORS" for details.

from __future__ import (absolute_import, division, print_function)

from ranger.colorschemes.default import Default
from ranger.gui.color import red, cyan, blue, white, normal


class Scheme(Default):

    def use(self, context):
        fg, bg, attr = Default.use(self, context)

        # if context.directory and not context.marked and not context.link \
        #         and not context.inactive_pane:
        #     fg = green

        # if context.in_titlebar and context.hostname:
        #     fg = red if context.bad else blue

        if context.video_linked and context.link and not context.marked:
            # fg = white if context.good else red
            if context.video_linked_badname:
                fg = red
            elif context.good:
                fg = blue if context.video_linked_tvshow else white
            else:
                fg = red
            # if context.directory:
            #     fg = blue
            if not context.selected:
                attr = normal

        if context.in_titlebar:
            if context.hostname:
                fg = red if context.bad else cyan
            attr = normal

        return fg, bg, attr
