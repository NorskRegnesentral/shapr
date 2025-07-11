# -*- coding: utf-8 -*-
# snapshottest: v1 - https://goo.gl/zC4yUc
from __future__ import unicode_literals

from snapshottest import Snapshot


snapshots = Snapshot()

snapshots['test_output_sage_xgb_gaussian test_output_sage_xgb_gaussian'] = '''function (y, pred) 
{
    loss <- mean((pred - y)^2)
    return(loss)
}
<bytecode: 0x55afa1f36f88>
<environment: 0x55afa7119960>
'''
