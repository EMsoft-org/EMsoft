#pragma once

#include <array>
#include <tuple>


#include <QtCore/QString>
#include <QtGui/QImage>


class EbsdLoader 
{
    public:
    EbsdLoader();
    ~EbsdLoader();
    EbsdLoader(const EbsdLoader&) = delete; // Copy Constructor Not Implemented
    EbsdLoader(EbsdLoader&&) = delete;      // Move Constructor Not Implemented
    EbsdLoader& operator=(const EbsdLoader&) = delete; // Copy Assignment Not Implemented
    EbsdLoader& operator=(EbsdLoader&&) = delete;      // Move Assignment Not Implemented

    static std::tuple<QImage, int32_t> CreateIPFColorMap(const QString& filepath, std::array<float, 3>& refDirection);



};

