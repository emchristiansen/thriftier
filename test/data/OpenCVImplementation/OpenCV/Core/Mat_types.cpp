/**
 * Autogenerated by Thrift Compiler (1.0.0-dev)
 *
 * DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
 *  @generated
 */
#include "OpenCV/Core/Mat_types.h"

#include <algorithm>



int _kCVTypeValues[] = {
  CVType::CV_8UC1,
  CVType::CV_8UC2,
  CVType::CV_8UC3,
  CVType::CV_8UC4,
  CVType::CV_8SC1,
  CVType::CV_8SC2,
  CVType::CV_8SC3,
  CVType::CV_8SC4,
  CVType::CV_16UC1,
  CVType::CV_16UC2,
  CVType::CV_16UC3,
  CVType::CV_16UC4,
  CVType::CV_16SC1,
  CVType::CV_16SC2,
  CVType::CV_16SC3,
  CVType::CV_16SC4,
  CVType::CV_32SC1,
  CVType::CV_32SC2,
  CVType::CV_32SC3,
  CVType::CV_32SC4,
  CVType::CV_32FC1,
  CVType::CV_32FC2,
  CVType::CV_32FC3,
  CVType::CV_32FC4,
  CVType::CV_64FC1,
  CVType::CV_64FC2,
  CVType::CV_64FC3,
  CVType::CV_64FC4
};
const char* _kCVTypeNames[] = {
  "CV_8UC1",
  "CV_8UC2",
  "CV_8UC3",
  "CV_8UC4",
  "CV_8SC1",
  "CV_8SC2",
  "CV_8SC3",
  "CV_8SC4",
  "CV_16UC1",
  "CV_16UC2",
  "CV_16UC3",
  "CV_16UC4",
  "CV_16SC1",
  "CV_16SC2",
  "CV_16SC3",
  "CV_16SC4",
  "CV_32SC1",
  "CV_32SC2",
  "CV_32SC3",
  "CV_32SC4",
  "CV_32FC1",
  "CV_32FC2",
  "CV_32FC3",
  "CV_32FC4",
  "CV_64FC1",
  "CV_64FC2",
  "CV_64FC3",
  "CV_64FC4"
};
const std::map<int, const char*> _CVType_VALUES_TO_NAMES(::apache::thrift::TEnumIterator(28, _kCVTypeValues, _kCVTypeNames), ::apache::thrift::TEnumIterator(-1, NULL, NULL));

const char* Mat::ascii_fingerprint = "5042C1F503B1892AB5DAD106DC89C2DB";
const uint8_t Mat::binary_fingerprint[16] = {0x50,0x42,0xC1,0xF5,0x03,0xB1,0x89,0x2A,0xB5,0xDA,0xD1,0x06,0xDC,0x89,0xC2,0xDB};

uint32_t Mat::read(::apache::thrift::protocol::TProtocol* iprot) {

  uint32_t xfer = 0;
  std::string fname;
  ::apache::thrift::protocol::TType ftype;
  int16_t fid;

  xfer += iprot->readStructBegin(fname);

  using ::apache::thrift::protocol::TProtocolException;

  bool isset_rows = false;
  bool isset_cols = false;
  bool isset_channels = false;
  bool isset_type = false;
  bool isset_data = false;

  while (true)
  {
    xfer += iprot->readFieldBegin(fname, ftype, fid);
    if (ftype == ::apache::thrift::protocol::T_STOP) {
      break;
    }
    switch (fid)
    {
      case -1:
        if (ftype == ::apache::thrift::protocol::T_I64) {
          xfer += iprot->readI64(this->rows);
          isset_rows = true;
        } else {
          xfer += iprot->skip(ftype);
        }
        break;
      case -2:
        if (ftype == ::apache::thrift::protocol::T_I64) {
          xfer += iprot->readI64(this->cols);
          isset_cols = true;
        } else {
          xfer += iprot->skip(ftype);
        }
        break;
      case -3:
        if (ftype == ::apache::thrift::protocol::T_I64) {
          xfer += iprot->readI64(this->channels);
          isset_channels = true;
        } else {
          xfer += iprot->skip(ftype);
        }
        break;
      case -4:
        if (ftype == ::apache::thrift::protocol::T_I64) {
          xfer += iprot->readI64(this->type);
          isset_type = true;
        } else {
          xfer += iprot->skip(ftype);
        }
        break;
      case -5:
        if (ftype == ::apache::thrift::protocol::T_STRING) {
          xfer += iprot->readBinary(this->data);
          isset_data = true;
        } else {
          xfer += iprot->skip(ftype);
        }
        break;
      default:
        xfer += iprot->skip(ftype);
        break;
    }
    xfer += iprot->readFieldEnd();
  }

  xfer += iprot->readStructEnd();

  if (!isset_rows)
    throw TProtocolException(TProtocolException::INVALID_DATA);
  if (!isset_cols)
    throw TProtocolException(TProtocolException::INVALID_DATA);
  if (!isset_channels)
    throw TProtocolException(TProtocolException::INVALID_DATA);
  if (!isset_type)
    throw TProtocolException(TProtocolException::INVALID_DATA);
  if (!isset_data)
    throw TProtocolException(TProtocolException::INVALID_DATA);
  return xfer;
}

uint32_t Mat::write(::apache::thrift::protocol::TProtocol* oprot) const {
  uint32_t xfer = 0;
  xfer += oprot->writeStructBegin("Mat");

  xfer += oprot->writeFieldBegin("data", ::apache::thrift::protocol::T_STRING, -5);
  xfer += oprot->writeBinary(this->data);
  xfer += oprot->writeFieldEnd();

  xfer += oprot->writeFieldBegin("type", ::apache::thrift::protocol::T_I64, -4);
  xfer += oprot->writeI64(this->type);
  xfer += oprot->writeFieldEnd();

  xfer += oprot->writeFieldBegin("channels", ::apache::thrift::protocol::T_I64, -3);
  xfer += oprot->writeI64(this->channels);
  xfer += oprot->writeFieldEnd();

  xfer += oprot->writeFieldBegin("cols", ::apache::thrift::protocol::T_I64, -2);
  xfer += oprot->writeI64(this->cols);
  xfer += oprot->writeFieldEnd();

  xfer += oprot->writeFieldBegin("rows", ::apache::thrift::protocol::T_I64, -1);
  xfer += oprot->writeI64(this->rows);
  xfer += oprot->writeFieldEnd();

  xfer += oprot->writeFieldStop();
  xfer += oprot->writeStructEnd();
  return xfer;
}

void swap(Mat &a, Mat &b) {
  using ::std::swap;
  swap(a.rows, b.rows);
  swap(a.cols, b.cols);
  swap(a.channels, b.channels);
  swap(a.type, b.type);
  swap(a.data, b.data);
}

const char* MatUnpacked::ascii_fingerprint = "E973DC0A09071677FCA82C37E29EFD3E";
const uint8_t MatUnpacked::binary_fingerprint[16] = {0xE9,0x73,0xDC,0x0A,0x09,0x07,0x16,0x77,0xFC,0xA8,0x2C,0x37,0xE2,0x9E,0xFD,0x3E};

uint32_t MatUnpacked::read(::apache::thrift::protocol::TProtocol* iprot) {

  uint32_t xfer = 0;
  std::string fname;
  ::apache::thrift::protocol::TType ftype;
  int16_t fid;

  xfer += iprot->readStructBegin(fname);

  using ::apache::thrift::protocol::TProtocolException;

  bool isset_rows = false;
  bool isset_cols = false;
  bool isset_channels = false;
  bool isset_data = false;

  while (true)
  {
    xfer += iprot->readFieldBegin(fname, ftype, fid);
    if (ftype == ::apache::thrift::protocol::T_STOP) {
      break;
    }
    switch (fid)
    {
      case -1:
        if (ftype == ::apache::thrift::protocol::T_I64) {
          xfer += iprot->readI64(this->rows);
          isset_rows = true;
        } else {
          xfer += iprot->skip(ftype);
        }
        break;
      case -2:
        if (ftype == ::apache::thrift::protocol::T_I64) {
          xfer += iprot->readI64(this->cols);
          isset_cols = true;
        } else {
          xfer += iprot->skip(ftype);
        }
        break;
      case -3:
        if (ftype == ::apache::thrift::protocol::T_I64) {
          xfer += iprot->readI64(this->channels);
          isset_channels = true;
        } else {
          xfer += iprot->skip(ftype);
        }
        break;
      case -4:
        if (ftype == ::apache::thrift::protocol::T_LIST) {
          {
            this->data.clear();
            uint32_t _size0;
            ::apache::thrift::protocol::TType _etype3;
            xfer += iprot->readListBegin(_etype3, _size0);
            this->data.resize(_size0);
            uint32_t _i4;
            for (_i4 = 0; _i4 < _size0; ++_i4)
            {
              xfer += iprot->readDouble(this->data[_i4]);
            }
            xfer += iprot->readListEnd();
          }
          isset_data = true;
        } else {
          xfer += iprot->skip(ftype);
        }
        break;
      default:
        xfer += iprot->skip(ftype);
        break;
    }
    xfer += iprot->readFieldEnd();
  }

  xfer += iprot->readStructEnd();

  if (!isset_rows)
    throw TProtocolException(TProtocolException::INVALID_DATA);
  if (!isset_cols)
    throw TProtocolException(TProtocolException::INVALID_DATA);
  if (!isset_channels)
    throw TProtocolException(TProtocolException::INVALID_DATA);
  if (!isset_data)
    throw TProtocolException(TProtocolException::INVALID_DATA);
  return xfer;
}

uint32_t MatUnpacked::write(::apache::thrift::protocol::TProtocol* oprot) const {
  uint32_t xfer = 0;
  xfer += oprot->writeStructBegin("MatUnpacked");

  xfer += oprot->writeFieldBegin("data", ::apache::thrift::protocol::T_LIST, -4);
  {
    xfer += oprot->writeListBegin(::apache::thrift::protocol::T_DOUBLE, static_cast<uint32_t>(this->data.size()));
    std::vector<double> ::const_iterator _iter5;
    for (_iter5 = this->data.begin(); _iter5 != this->data.end(); ++_iter5)
    {
      xfer += oprot->writeDouble((*_iter5));
    }
    xfer += oprot->writeListEnd();
  }
  xfer += oprot->writeFieldEnd();

  xfer += oprot->writeFieldBegin("channels", ::apache::thrift::protocol::T_I64, -3);
  xfer += oprot->writeI64(this->channels);
  xfer += oprot->writeFieldEnd();

  xfer += oprot->writeFieldBegin("cols", ::apache::thrift::protocol::T_I64, -2);
  xfer += oprot->writeI64(this->cols);
  xfer += oprot->writeFieldEnd();

  xfer += oprot->writeFieldBegin("rows", ::apache::thrift::protocol::T_I64, -1);
  xfer += oprot->writeI64(this->rows);
  xfer += oprot->writeFieldEnd();

  xfer += oprot->writeFieldStop();
  xfer += oprot->writeStructEnd();
  return xfer;
}

void swap(MatUnpacked &a, MatUnpacked &b) {
  using ::std::swap;
  swap(a.rows, b.rows);
  swap(a.cols, b.cols);
  swap(a.channels, b.channels);
  swap(a.data, b.data);
}

