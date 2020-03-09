from ctypes import *
import random
import logging
import math
import re

def str_to_hex_list(text):
    strWithOutSpace = text.replace(" ", "")
    strWithOutSpace = strWithOutSpace.replace("\n", "")
    pattern = "[^a-fA-F0-9]"
    if len(re.findall(pattern, strWithOutSpace)) != 0:
        return None
    length = len(strWithOutSpace)
    if not (length % 2 == 0):
        return None
    length = int(length / 2)
    hexList = []
    try:
        for i in range(length):
            temp = strWithOutSpace[i * 2:i * 2 + 2]
            hexList.append(int(temp, 16))
        return hexList
    except:
        return None

class CBigInt_Tag(Structure):
    _fields_ =[('m_nLength', c_uint),
               ('m_ulValue', c_ulong*32)]

class SM9Params(Structure):
    _fields_=[('t',CBigInt_Tag),
              ('q', CBigInt_Tag),
              ('n', CBigInt_Tag),
              ('tr', CBigInt_Tag),
              ('ord', CBigInt_Tag),
              ('exp_6t_5', CBigInt_Tag),
              ('exp_6t_2_1', CBigInt_Tag),
              ('q_1_12', CBigInt_Tag),
              ('q12_1_12', CBigInt_Tag),
              ('exp_64', CBigInt_Tag),
              ('exp__8', CBigInt_Tag),
              ('exp__2', CBigInt_Tag),
              ('exp_4', CBigInt_Tag),
              ('ZERO', CBigInt_Tag),
              ('ONE', CBigInt_Tag),
              ]

class BNField2(Structure):
    _fields_=[('re',CBigInt_Tag),
    ('im', CBigInt_Tag)]

class BNPoint2(Structure):
    _fields_=[('x',BNField2),
    ('y', BNField2),
    ('z', BNField2)]

class BNPoint(Structure):
    _fields_ = [('x', CBigInt_Tag),
                ('y', CBigInt_Tag),
                ('z', CBigInt_Tag)]

class BNField4(Structure):
    _fields_ = [('re', BNField2),
                ('im', BNField2)]

class BNField12(Structure):
    _fields_ = [('re', BNField4),
                ('im', BNField4),
                ('sq', BNField4)]

class sm4_context(Structure):
    _fields_ =[('mode', c_int),
               ('sk', c_ulong*32)]

def addzero(str):
    if len(str)==8:
        return str
    add_zero_num=8-len(str)
    zero_str=''
    for i in range(add_zero_num):
        zero_str=zero_str+'0'
    return zero_str+str

def addzero64(str):
    if len(str)==64:
        return str
    add_zero_num=64-len(str)
    zero_str=''
    for i in range(add_zero_num):
        zero_str=zero_str+'0'
    return zero_str+str

def CBigInt_Tag_str(CBigInt_Tag):
    res=''
    for i in range(CBigInt_Tag.m_nLength):
        res=res+addzero(hex(CBigInt_Tag.m_ulValue[CBigInt_Tag.m_nLength-1-i])[2:])
    return res.upper()

def BNPoint_str(BnPoint):
    x=''
    y=''
    for i in range(BnPoint.x.m_nLength):
        x=x+addzero(hex(BnPoint.x.m_ulValue[BnPoint.x.m_nLength-1-i])[2:])
    for i in range(BnPoint.y.m_nLength):
        y=y+addzero(hex(BnPoint.y.m_ulValue[BnPoint.y.m_nLength-1-i])[2:])
    return x.upper()+'\n'+y.upper()

def BNPoint_byte(BnPoint):
    x=''
    y=''
    for i in range(BnPoint.x.m_nLength):
        x=x+addzero(hex(BnPoint.x.m_ulValue[BnPoint.x.m_nLength-1-i])[2:])
    for i in range(BnPoint.y.m_nLength):
        y=y+addzero(hex(BnPoint.y.m_ulValue[BnPoint.y.m_nLength-1-i])[2:])
    return x+y

def BNField2_str(BnField):
    x=''
    y=''
    for i in range(BnField.im.m_nLength):
        x=x+addzero(hex(BnField.im.m_ulValue[BnField.im.m_nLength-1-i])[2:])
    for i in range(BnField.re.m_nLength):
        y=y+addzero(hex(BnField.re.m_ulValue[BnField.re.m_nLength-1-i])[2:])
    return 'x:'+x.upper()+'\n'+'y:'+y.upper()

def BNField2_byte(BnField):
    x=''
    y=''
    for i in range(BnField.im.m_nLength):
        x=x+addzero(hex(BnField.im.m_ulValue[BnField.im.m_nLength-1-i])[2:])
    for i in range(BnField.re.m_nLength):
        y=y+addzero(hex(BnField.re.m_ulValue[BnField.re.m_nLength-1-i])[2:])
    return x+y

def BNField4_byte(BnField4):
    x=BNField2_byte(BnField4.im)
    y=BNField2_byte(BnField4.re)
    return x+y

def BNField12_byte(BnField12):
    x=BNField4_byte(BnField12.re)
    y=BNField4_byte(BnField12.im)
    z=BNField4_byte(BnField12.sq)
    return z+y+x

def BNPoint2_str(BNPoint2):
    x_1=''
    x_2=''
    y_1=''
    y_2=''
    for i in range(BNPoint2.x.re.m_nLength):
        x_1=x_1+addzero(hex(BNPoint2.x.re.m_ulValue[BNPoint2.x.re.m_nLength-1-i])[2:])

    for i in range(BNPoint2.x.im.m_nLength):
        x_2=x_2+addzero(hex(BNPoint2.x.im.m_ulValue[BNPoint2.x.im.m_nLength-1-i])[2:])

    for i in range(BNPoint2.y.re.m_nLength):
        y_1=y_1+addzero(hex(BNPoint2.y.re.m_ulValue[BNPoint2.y.re.m_nLength-1-i])[2:])

    for i in range(BNPoint2.y.im.m_nLength):
        y_2=y_2+addzero(hex(BNPoint2.y.im.m_ulValue[BNPoint2.y.im.m_nLength-1-i])[2:])

    return x_2.upper()+'\n'+x_1.upper()+'\n'+y_2.upper()+'\n'+y_1.upper()

def SK_str(SK,k_len):
    str=''
    for i in range(k_len):
        if len(hex(ord(SK[i]))[2:])==1:
            str=str+'0'
        str=str+hex(ord(SK[i]))[2:]
    return str.upper()

def Byte_str(byte,len_byte):
    ret=''
    for i in range(len_byte):
        if len(hex(ord(byte[i]))[2:])==1:
            ret=ret+'0'+hex(ord(byte[i]))[2:]
        else:
            ret = ret+ hex(ord(byte[i]))[2:]
    return ret

def construct_BNPoint(x_str,y_str):
    x = CBigInt_Tag()
    y = CBigInt_Tag()
    bn=BNPoint()
    dll.Get(byref(x), x_str.encode(), 16)
    dll.Get(byref(y), y_str.encode(), 16)
    dll.P_construct_xy(byref(bn), x, y)
    return bn

def construct_BNPoint2(a_str,b_str,c_str,d_str):
    bn_a = CBigInt_Tag()
    bn_b = CBigInt_Tag()
    bn_c = CBigInt_Tag()
    bn_d = CBigInt_Tag()
    bn_x = BNField2()
    bn_y = BNField2()
    bn = BNPoint2()

    dll.Get(byref(bn_a), a_str.encode(), 16)
    dll.Get(byref(bn_b), b_str.encode(), 16)
    dll.Get(byref(bn_c), c_str.encode(), 16)
    dll.Get(byref(bn_d), d_str.encode(), 16)

    dll.F2_construct(byref(bn_x), bn_b, bn_a)
    dll.F2_construct(byref(bn_y), bn_d, bn_c)
    dll.P2_construct_xy(byref(bn), bn_x, bn_y)
    return bn

def hexstr_char(hexstr):
    str = ''
    for i in range(int(len(hexstr)/2)):
        str=str+chr(int(hexstr[2*i:2*i+2],16))
    return str

dll = CDLL('sm9.dll')

class CryptSM9_Encryption(object):
    def __init__(self, ke_str='01EDEE3778F441F8DEA3D9FA0ACC4E07EE36C93F9A08618AF4AD85CEDE1C22', id_str='Bob'):
        self.__ke_str = ke_str
        self.__id_str = id_str
        self.initParameters()

    def initParameters(self):
        # Initialization parameters
        a = CBigInt_Tag()
        b = CBigInt_Tag()
        c = CBigInt_Tag()
        d = CBigInt_Tag()
        e = CBigInt_Tag()
        f = CBigInt_Tag()
        b1 = BNField2()
        b2 = BNField2()

        self.ke = CBigInt_Tag()
        self.r = CBigInt_Tag()
        self.P2 = BNPoint2()
        self.deB = BNPoint2()
        self.P1 = BNPoint()
        self.P_pub_e = BNPoint()
        self.C1 = BNPoint()
        self.C2 = ''
        self.C3 = ''
        self.w = BNField12()
        self.ctx = sm4_context()

        # Initialization curve
        BN = SM9Params()
        dll.SM9Params_init(byref(BN))
        dll.Get(byref(a), b'93DE051D62BF718FF5ED0704487D01D6E1E4086909DC3280E8C4E4817C66DDDD', 16)
        dll.Get(byref(b), b'21FE8DDA4F21E607631065125C395BBC1C1C00CBFA6024350C464CD70A3EA616', 16)
        dll.Get(byref(c), b'85AEF3D078640C98597B6027B441A01FF1DD2C190F5E93C454806C11D8806141', 16)
        dll.Get(byref(d), b'3722755292130B08D2AAB97FD34EC120EE265948D19C17ABF9B7213BAF82D65B', 16)
        dll.Get(byref(e), b'17509B092E845C1266BA0D262CBEE6ED0736A96FA347C8BD856DC76B84EBEB96', 16)
        dll.Get(byref(f), b'A7CF28D519BE3DA65F3170153D278FF247EFBA98A71A08116215BBA5C999A7C7', 16)

        dll.P_construct_xy(byref(self.P1), a, b)
        dll.F2_construct(byref(b1), d, c)
        dll.F2_construct(byref(b2), f, e)
        dll.P2_construct_xy(byref(self.P2), b1, b2)

    def generate_key(self):
        try:
            dll.Get(byref(self.ke), self.__ke_str.encode(), 16)
            dll.P_multiply(byref(self.P_pub_e), self.P1, self.ke)
            dll.P_normorlize(byref(self.P_pub_e), self.P_pub_e)
            dll.PKC_Keygen(byref(self.deB), self.ke, self.__id_str.encode(), self.P2)
            dll.P2_normorlize(byref(self.deB), self.deB)
            self.de_str = BNPoint2_str(self.deB)
            self.Ppub_str = BNPoint_str(self.P_pub_e)
            return BNPoint_str(self.P_pub_e),BNPoint2_str(self.deB)

        except Exception as e:
            logging.debug(e)

    def char_arr(self,len):
        T_char_arr = c_char * int(len)  # ctypes.c_char_Array_3
        return T_char_arr()

    def encrypt(self, r_str='0000AAC0541779C8FC45E3E2CB25C12B5D2576B2129AE8BB5EE2CBE5EC9E785C', msg_bytes='4368696E 65736520 49424520 7374616E 64617264', mode='block'):
        try:
            a_b_c_d = self.de_str.split('\n')
            self.deB = construct_BNPoint2(a_b_c_d[0], a_b_c_d[1], a_b_c_d[2], a_b_c_d[3])
            x_y = self.Ppub_str.split('\n')
            self.P_pub_e = construct_BNPoint(x_y[0], x_y[1])

            if not dll.P2_isOnBNTCurve(self.deB):
                print('deB is not on the curve.\n')
                return
            if not dll.P_isOnBNCurve(self.P_pub_e):
                print('P_pub_e is not on the curve.\n')
                return

            msg_bytes=bytes(str_to_hex_list(msg_bytes))
            mLen = len(msg_bytes)

            dll.Get(byref(self.r), r_str.encode(), 16)
            K2_len = 256

            if mode == 'stream':
                K2_len = K2_len / 8
                klen = int((mLen + K2_len) * 8)
                K = self.char_arr(klen / 8)
                dll.PKC_kem(byref(K), byref(self.C1), self.__id_str.encode(), self.P1, self.P2, self.P_pub_e, int(klen),
                            self.r)
                K1 = self.char_arr(mLen)
                for i in range(mLen):
                    K1[i] = K[i]
                K2 = self.char_arr(K2_len)
                for i in range(int(K2_len)):
                    K2[i] = K[i + mLen]
                self.C2 = self.char_arr(mLen)
                dll.Bytes_XOR(byref(self.C2), byref(K1), msg_bytes, mLen)
                self.C3 = self.char_arr(32)
                dll.MAC(byref(self.C3), byref(self.C2), mLen, byref(K2), int(K2_len))
                return (BNPoint_byte(self.C1).upper() + Byte_str(self.C3, 32).upper() + Byte_str(self.C2,
                                                                                                 mLen).upper())
            elif mode == 'block':
                K1_len = 0x80
                klen = K1_len + K2_len
                K = self.char_arr(klen)
                dll.PKC_kem(byref(K), byref(self.C1), self.__id_str.encode(), self.P1, self.P2, self.P_pub_e, int(klen),
                            self.r)
                K1_len = int(K1_len / 8)
                K2_len = int(K2_len / 8)
                K1 = self.char_arr(K1_len)
                for i in range(K1_len):
                    K1[i] = K[i]
                K2 = self.char_arr(K2_len)
                for i in range(int(K2_len)):
                    K2[i] = K[i + K1_len]
                if mLen % 16==0:
                    block_size = mLen/16
                else:
                    block_size = (math.floor(mLen/16)+1)*16
                input = self.char_arr(block_size)
                for i in range(block_size):
                    if i < mLen:
                        input[i] = msg_bytes[i]
                    else:
                        input[i] = 0x0C
                self.C2 = self.char_arr(block_size)
                dll.sm4_setkey_enc(byref(self.ctx), K1)
                dll.sm4_crypt_ecb(byref(self.ctx), 1, block_size, byref(input), byref(self.C2))
                self.C3 = self.char_arr(32)
                dll.MAC(byref(self.C3), byref(self.C2), 32, byref(K2), K2_len)
                return (BNPoint_byte(self.C1).upper() + Byte_str(self.C3, 32).upper() + Byte_str(self.C2, block_size).upper())
        except Exception as e:
            logging.debug(e)

    def __get_cipher(self, ciphertext, mode):
        # C1 128 C3=64
        if mode == 'stream':

            if len(ciphertext) <= 128 + 64:
                print(
                    'The length of Ciphertext should be more than 192, now the length is ' + str(len(ciphertext)) + '.\n')
                return False

            C1 = ciphertext[:128]
            C3 = ciphertext[128:128 + 64]
            C2 = ciphertext[128 + 64:]
            C1_x = CBigInt_Tag()
            C1_y = CBigInt_Tag()
            dll.Get(byref(C1_x), C1[:64].encode(), 16)
            dll.Get(byref(C1_y), C1[64:128].encode(), 16)
            dll.P_construct_xy(byref(self.C1), C1_x, C1_y)
            self.C3 = self.char_arr(32)
            len_C2 = int((len(ciphertext) - 128 - 64) / 2)
            self.C2 = self.char_arr(len_C2)
            for i in range(32):
                self.C3[i] = int(C3[2 * i:2 * i + 2], 16)
            for i in range(len_C2):
                self.C2[i] = int(C2[2 * i:2 * i + 2], 16)
        else:
            if len(ciphertext) <= 128 + 64:
                print(
                    'The length of Ciphertext should be more than 192, now the length is ' + str(len(ciphertext)) + '.\n')
                return False
            block_size=int((len(ciphertext)-128-64)/2)

            C1 = ciphertext[:128]
            C3 = ciphertext[128:128 + 64]
            C2 = ciphertext[128 + 64:]
            C1_x = CBigInt_Tag()
            C1_y = CBigInt_Tag()
            dll.Get(byref(C1_x), C1[:64].encode(), 16)
            dll.Get(byref(C1_y), C1[64:128].encode(), 16)
            dll.P_construct_xy(byref(self.C1), C1_x, C1_y)
            self.C3 = self.char_arr(32)
            self.C2 = self.char_arr(block_size)
            for i in range(32):
                self.C3[i] = int(C3[2 * i:2 * i + 2], 16)
            for i in range(block_size):
                self.C2[i] = int(C2[2 * i:2 * i + 2], 16)
        return True

    def decrypt(self, cipher = '2445471164490618E1EE20528FF1D545B0F14C8BCAA44544F03DAB5DAC07D8FF42FFCA97D57CDDC05EA405F2E586FEB3A6930715532B8000759F13059ED59AC0BA672387BCD6DE5016A158A52BB2E7FC429197BCAB70B25AFEE37A2B9DB9F3671B5F5B0E951489682F3E64E1378CDD5DA9513B1C',
                mode='block'):
        try:
            if not self.__get_cipher(cipher,mode):
                return

            a_b_c_d = self.de_str.split('\n')
            self.deB = construct_BNPoint2(a_b_c_d[0], a_b_c_d[1], a_b_c_d[2], a_b_c_d[3])
            x_y = self.Ppub_str.split('\n')
            self.P_pub_e = construct_BNPoint(x_y[0], x_y[1])

            if not dll.P2_isOnBNTCurve(self.deB):
                print('deB is not on the curve.\n')
                return
            if not dll.P_isOnBNCurve(self.P_pub_e):
                print('P_pub_e is not on the curve.\n')
                return

            dll.Pairing_opt(byref(self.w), self.deB, self.C1)
            msg_len = int((len(cipher)-128-64)/2)
            idLen = len(self.__id_str)

            len_msg = 64 + 384 + idLen
            msg = self.char_arr(len_msg)
            dll.PtoByte(byref(msg), self.C1)
            msg1 = self.char_arr(384)
            dll.F12toByte(byref(msg1), self.w)

            for i in range(384):
                msg[64 + i] = msg1[i]
            for i in range(idLen):
                msg[64 + 384 + i] = self.__id_str[i].encode()

            K2_len = 256

            if mode == 'stream':
                K2_len = K2_len / 8
                klen = int((msg_len + K2_len) * 8)
                K_ = self.char_arr(klen / 8)
                dll.KDF(K_, msg, len_msg, int(klen))

                K2_len = int(K2_len / 8)
                K1_ = self.char_arr(msg_len)
                for i in range(msg_len):
                    K1_[i] = K_[i]

                K2_ = self.char_arr(K2_len)
                for i in range(int(K2_len)):
                    K2_[i] = K_[i + msg_len]

                M_ = self.char_arr(msg_len)
                dll.Bytes_XOR(byref(M_), K1_, self.C2, msg_len)
                return Byte_str(M_, msg_len).upper()

            elif mode == 'block':
                K1_len = 0x80
                klen = K1_len + K2_len
                K_ = self.char_arr(klen / 8)
                dll.KDF(K_, msg, len_msg, int(klen))

                K1_len = int(K1_len / 8)
                K2_len = int(K2_len / 8)

                K1_ = self.char_arr(K1_len)
                for i in range(K1_len):
                    K1_[i] = K_[i]

                K2_ = self.char_arr(K2_len)
                for i in range(K2_len):
                    K2_[i] = K_[i + K1_len]

                block_size=len(self.C2)

                M_ = self.char_arr(block_size)
                dll.sm4_setkey_dec(byref(self.ctx), K1_)
                dll.sm4_crypt_ecb(byref(self.ctx), 0, block_size, byref(self.C2), M_)
                return Byte_str(M_, msg_len).upper()
        except Exception as e:
            logging.debug(e)

class CryptSM9_Signature(object):
    def __init__(self, ks_str='0130E78459D78545CB54C587E02CF480CE0B66340F319F348A1D5B1F2DC5F4', id_str='Alice'):
        self.__ks_str=ks_str
        self.__id_str=id_str
        self.initParameters()

    def initParameters(self):
        a = CBigInt_Tag()
        b = CBigInt_Tag()
        c = CBigInt_Tag()
        d = CBigInt_Tag()
        e = CBigInt_Tag()
        f = CBigInt_Tag()

        self.ks = CBigInt_Tag()
        self.h = CBigInt_Tag()
        self.h2 = CBigInt_Tag()
        self.r = CBigInt_Tag()

        b1 = BNField2()
        b2 = BNField2()
        self.P2 = BNPoint2()
        self.P_pub_s = BNPoint2()

        self.P1 = BNPoint()
        self.dsA = BNPoint()
        self.S = BNPoint()

        # Initialization curve
        BN = SM9Params()
        dll.SM9Params_init(byref(BN))
        dll.Get(byref(a), b'93DE051D62BF718FF5ED0704487D01D6E1E4086909DC3280E8C4E4817C66DDDD', 16)
        dll.Get(byref(b), b'21FE8DDA4F21E607631065125C395BBC1C1C00CBFA6024350C464CD70A3EA616', 16)
        dll.Get(byref(c), b'85AEF3D078640C98597B6027B441A01FF1DD2C190F5E93C454806C11D8806141', 16)
        dll.Get(byref(d), b'3722755292130B08D2AAB97FD34EC120EE265948D19C17ABF9B7213BAF82D65B', 16)
        dll.Get(byref(e), b'17509B092E845C1266BA0D262CBEE6ED0736A96FA347C8BD856DC76B84EBEB96', 16)
        dll.Get(byref(f), b'A7CF28D519BE3DA65F3170153D278FF247EFBA98A71A08116215BBA5C999A7C7', 16)

        dll.P_construct_xy(byref(self.P1), a, b)
        dll.F2_construct(byref(b1), d, c)
        dll.F2_construct(byref(b2), f, e)
        dll.P2_construct_xy(byref(self.P2), b1, b2)

    def generate_key(self):
        try:
            dll.Get(byref(self.ks), self.__ks_str.encode(), 16)
            dll.P2_multiply(byref(self.P_pub_s), self.P2, self.ks)
            dll.P2_normorlize(byref(self.P_pub_s), self.P_pub_s)
            dll.DSA_Keygen(byref(self.dsA), self.ks, self.__id_str.encode(), self.P1)
            dll.P_normorlize(byref(self.dsA), self.dsA)
            self.Pub_str = BNPoint2_str(self.P_pub_s)
            self.dsA_str = BNPoint_str(self.dsA)
            return BNPoint2_str(self.P_pub_s),BNPoint_str(self.dsA)

        except Exception as e:
            logging.debug(e)

    def sign(self, msg_bytes="4368696E65736520494253207374616E64617264", r_str="033C8616B06704813203DFD00965022ED15975C662337AED648835DC4B1CBE"):
        try:
            Pub_str_list = self.Pub_str.split('\n')
            dsA_str_list = self.dsA_str.split('\n')
            self.dsA = construct_BNPoint(dsA_str_list[0], dsA_str_list[1])
            self.P_pub_s = construct_BNPoint2(Pub_str_list[0], Pub_str_list[1], Pub_str_list[2], Pub_str_list[3])
            if not dll.P2_isOnBNTCurve(self.P_pub_s):
                print('Ppub is not on the curve.\n')
                return
            if not dll.P_isOnBNCurve(self.dsA):
                print('ds is not on the curve.\n')
                return
            dll.Get(byref(self.r), r_str.encode(), 16)
            msg_bytes = bytes(str_to_hex_list(msg_bytes))
            dll.DSA_Sign(byref(self.h), byref(self.S), msg_bytes, self.P1, self.P_pub_s, self.dsA, self.r)
            return CBigInt_Tag_str(self.h),BNPoint_str(self.S)

        except Exception as e:
            logging.debug(e)

    def verify(self, msg_bytes='4368696E65736520494253207374616E64617264', h_str='823C4B21E4BD2DFE1ED92C606653E996668563152FC33F55D7BFBB9BD9705ADB', S_str='73BF96923CE58B6AD0E13E9643A406D8EB98417C50EF1B29CEF9ADB48B6D598C\n\
856712F1C2E0968AB7769F42A99586AED139D5B8B3E15891827CC2ACED9BAA05'):
        try:
            Pub_str_list = self.Pub_str.split('\n')
            self.P_pub_s = construct_BNPoint2(Pub_str_list[0], Pub_str_list[1], Pub_str_list[2], Pub_str_list[3])

            if not dll.P2_isOnBNTCurve(self.P_pub_s):
                print('Ppub is not on the curve.\n')
                return
            dll.Get(byref(self.h), h_str.encode(), 16)
            S_str_list=S_str.split('\n')
            self.S=construct_BNPoint(S_str_list[0],S_str_list[1])
            msg_bytes=bytes(str_to_hex_list(msg_bytes))
            res = dll.DSA_Verify(byref(self.h2), self.h, self.S, msg_bytes, self.__id_str.encode(), self.P1, self.P2, self.P_pub_s)
            return res

        except Exception as e:
            logging.debug(e)

if __name__ == '__main__':

    sm9_enc = CryptSM9_Encryption()
    Ppub, de = sm9_enc.generate_key()

    cipher= sm9_enc.encrypt(mode='block', msg_bytes='4368696E 65736520 49424520 7374616E 64617264')
    print(cipher)

    plaintext=sm9_enc.decrypt(cipher=cipher, mode='block')
    print(plaintext)

    sm9_sig = CryptSM9_Signature()
    Ppub, ds = sm9_sig.generate_key()
    print('Ppub', Ppub)
    print('ds', ds)

    h, S=sm9_sig.sign()
    print('h', h)
    print('S', S)

    res=sm9_sig.verify()
    print(res)







