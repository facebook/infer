# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
import dir1.dir4.testmod as Imported
from dir1.dir3.testmod import D as ImportedD


def read_class_companion_property_with_import_bad():
    my_dict = {'key2': 0, "key": 1}
    return my_dict[Imported.C.class_property]


def read_class_companion_property_with_import_ok():
    my_dict = {'testmod.C.class_property': 0, "key": 1}
    return my_dict[Imported.C.class_property]


def instance_attribute_from_class_companion_with_import_bad():
    my_dict = {'key2': 0, "key": 1}
    o = Imported.C()
    return my_dict[o.class_property]


def instance_attribute_from_class_companion_with_import_ok():
    my_dict = {'testmod.C.class_property': 0, "key": 1}
    o = Imported.C()
    return my_dict[o.class_property]


def instance_attribute_with_import_bad():
    my_dict = {'key2': 0, "key": 1}
    o = Imported.C()
    return my_dict[o.property]


def instance_attribute_with_import_ok():
    my_dict = {'testmod.C.instance_property': 0, "key": 1}
    o = Imported.C()
    return my_dict[o.property]


def read_class_companion_property_with_import_from_bad():
    my_dict = {'key2': 0, "key": 1}
    return my_dict[ImportedD.class_property]


def read_class_companion_property_with_import_from_ok():
    my_dict = {'testmod.D.class_property': 0, "key": 1}
    return my_dict[ImportedD.class_property]


def instance_attribute_from_class_companion_with_import_from_bad():
    my_dict = {'key2': 0, "key": 1}
    o = ImportedD()
    return my_dict[o.class_property]


def instance_attribute_from_class_companion_with_import_from_ok():
    my_dict = {'testmod.D.class_property': 0, "key": 1}
    o = ImportedD()
    return my_dict[o.class_property]


def instance_attribute_with_import_from_bad():
    my_dict = {'key2': 0, "key": 1}
    o = ImportedD()
    return my_dict[o.property]


def instance_attribute_with_import_from_ok():
    my_dict = {'testmod.D.instance_property': 0, "key": 1}
    o = ImportedD()
    return my_dict[o.property]
