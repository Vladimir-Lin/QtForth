NAME         = QtForth
TARGET       = $${NAME}

QT           = core
QT          -= gui
QT          += network
QT          += sql
QT          += script
QT          += Essentials
QT          += Mathematics

load(qt_build_config)
load(qt_module)

INCLUDEPATH += $${PWD}/../../include/$${NAME}

HEADERS     += $${PWD}/../../include/$${NAME}/qtforth.h

SOURCES     += $${PWD}/nForthNames.cpp
SOURCES     += $${PWD}/nForthCodes.cpp
SOURCES     += $${PWD}/nForth.cpp
SOURCES     += $${PWD}/nForthUtils.cpp
# SOURCES     += $${PWD}/nForthBind.cpp

OTHER_FILES += $${PWD}/../../include/$${NAME}/headers.pri

include ($${PWD}/../../doc/Qt/Qt.pri)

TRNAME       = $${NAME}
include ($${PWD}/../../Translations.pri)
