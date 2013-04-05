'''Klarna API - Address

Provides Address class which holds a single address of a person or company
'''

# Copyright 2011 KLARNA AB. All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#    1. Redistributions of source code must retain the above copyright notice,
#       this list of conditions and the following disclaimer.
#
#    2. Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY KLARNA AB "AS IS" AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
# EVENT SHALL KLARNA AB OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
# OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
# EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# The views and conclusions contained in the software and documentation are
# those of the authors and should not be interpreted as representing official
# policies, either expressed or implied, of KLARNA AB.

# python3k campatibility
from __future__ import print_function
import sys

__all__ = ('Address',)

from .error import KlarnaException, UnknownCountry
from .const import *
from .util import basestring

import re


class Address(object):
    fields = (
        'email', 'telno', 'cellno', 'fname', 'lname', 'company', 'careof',
        'street', 'house_number', 'house_extension', 'zip', 'city', 'country'
    )

    @property
    def email(self):
        'Email address'
        return self._email

    @email.setter
    def email(self, value):
        self._email = value

    @property
    def telno(self):
        'Phone number'
        return self._telno

    @telno.setter
    def telno(self, value):
        self._telno = value

    @property
    def cellno(self):
        'Cellphone number'
        return self._cellno

    @cellno.setter
    def cellno(self, value):
        self._cellno = value

    @property
    def fname(self):
        'First name'
        return self._fname

    @fname.setter
    def fname(self, value):
        self._fname = value

    @property
    def lname(self):
        'Last name'
        return self._lname

    @lname.setter
    def lname(self, value):
        self._lname = value

    @property
    def company(self):
        'Company name'
        return self._company

    @company.setter
    def company(self, value):
        self.is_company = True
        self._company = value

    @property
    def careof(self):
        'Care of, C/O'
        return self._careof

    @careof.setter
    def careof(self, value):
        self._careof = value

    @property
    def street(self):
        'Street address'
        return self._street

    @street.setter
    def street(self, value):
        self._street = value

    @property
    def zip(self):
        'Zip Code'
        return self._zip

    @zip.setter
    def zip(self, value):
        self._zip = value

    @property
    def city(self):
        'City'
        return self._city

    @city.setter
    def city(self, value):
        self._city = value

    @property
    def country(self):
        'Country code'
        return self._country

    @country.setter
    def country(self, value):
        try:
            self._country = lookup(Countries, value)
        except ValueError:
            raise UnknownCountry(value)

    @property
    def house_number(self):
        'House number'
        return self._house_number

    @house_number.setter
    def house_number(self, value):
        self._house_number = value

    @property
    def house_extension(self):
        'House Extension'
        return self._house_extension

    @house_extension.setter
    def house_extension(self, value):
        self._house_extension = value

    def __init__(self, **kwargs):
        self.is_company = False
        for k, v in kwargs.items():
            if v is not None:
                setattr(self, k, v)

    def __iter__(self):
        for k in self.fields:
            if hasattr(self, k):
                yield k

    def __getitem__(self, key):
        return getattr(self, key, '')

    def __contains__(self, key):
        return getattr(self, key, None)

    def __repr__(self):
        return '(Addr %s)' % ', '.join(
            ['%s = %r' %
                (k, getattr(self, k)) for k in self.fields if hasattr(self, k)])
''' Klarna API - Calc

All rates are yearly rates, but they are calculated monthly. So
a rate of 9 % is used 0.75% monthly. The first is the one we specify
to the customers, and the second one is the one added each month to
the account. The IRR uses the same notation.

The APR is however calculated by taking the monthly rate and raising
it to the 12 power. This is according to the EU law, and will give
very large numbers if the $pval is small compared to the $fee and
the amount of months you repay is small as well.

All functions work in discrete mode, and the time interval is the
mythical evenly divided month. There is no way to calculate APR in
days without using integrals and other hairy math. So don't try.
The amount of days between actual purchase and the first bill can
of course vary between 28 and 61 days, but all calculations in this
class assume this time is exactly and that is ok since this will only
overestimate the APR and all examples in EU law uses whole months as well.
Provides Address class which holds a single address of a person or company
'''

# Copyright 2011 KLARNA AB. All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#    1. Redistributions of source code must retain the above copyright notice,
#       this list of conditions and the following disclaimer.
#
#    2. Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY KLARNA AB "AS IS" AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
# EVENT SHALL KLARNA AB OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
# OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
# EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# The views and conclusions contained in the software and documentation are
# those of the authors and should not be interpreted as representing official
# policies, either expressed or implied, of KLARNA AB.

__all__ = ('calc_apr',
    'total_credit_purchase_cost',
    'calc_monthly_cost',
    'get_lowest_payment_for_account',
    'pround')

from math import log, ceil
from .error import KlarnaException
from .const import Page
from .pclass import PClass

accuracy = 0.01


def midpoint(a, b):
    return (a + b) / 2


def npv(pval, payarray, rate, fromdayone):
    ''' npv - Net Present Value
    '''

    for i, payment in enumerate(payarray):
        pval -= payment / pow(1 + rate / (12 * 100.0), fromdayone + i)

    return pval


def irr(pval, payarray, fromdayone):
    ''' irr - Internal Rate of Return
        This function uses divide and conquer to numerically find the IRR
    '''

    low = 0.0
    high = 100.0
    lowval = npv(pval, payarray, low, fromdayone)
    highval = npv(pval, payarray, high, fromdayone)

    if lowval > 0.0:
        return -1  # raise Exception ?

    while high < 1000000:
        mid = midpoint(low, high)
        midval = npv(pval, payarray, mid, fromdayone)
        if abs(midval) < accuracy:
            # Close enough
            return mid

        if highval < 0:
            # not in range, double it
            low, lowval = high, highval
            high *= 2
            highval = npv(pval, payarray, high, fromdayone)
        elif midval >= 0:
            # irr is between low and mid
            high, highval = mid, midval
        else:
            # irr is between mid and high
            low, lowval = mid, midval

    return -2


def irr2apr(irr):
    ''' Turns IRR into APR

        IRR is not the same thing as APR, Annual Percentage Rate. The
        IRR is per time period, i.e. 1 month, and the APR is per year,
        and note that that you need to raise to the power of 12, not
        mutliply by 12.
    '''
    return (100 * (pow(1 + irr / (12 * 100.0), 12) - 1))


def fulpacc(pval, rate, fee, minpay, payment, months, base):
    ''' This is a simplified model of how our paccengine works if
        a client always pays their bills. It adds interest and fees
        and checks minimum payments. It will run until the value
        of the account reaches 0, and return an array of all the
        individual payments. Months is the amount of months to run
        the simulation. Important! Don't feed it too few months or
        the whole loan won't be paid off, but the other functions
        should handle this correctly.

        Giving it too many months has no bad effects, or negative
        amount of months which means run forever, but it will stop
        as soon as the account is paid in full.

        Depending if the account is a base account or not, the
        Payment has to be 1/24 of the capital amount.

        The payment has to be at least $minpay, unless the capital
        amount + interest + fee is less than $minpay; in that case
        that amount is paid and the function returns since the client
        no longer owes any money.
    '''

    bal = pval
    payarray = []
    while months != 0 and bal > accuracy:
        interest = bal * rate / (100.0 * 12)
        nbal = bal + interest + fee

        if minpay >= nbal or payment >= nbal:
            payarray.append(nbal)
            return payarray

        npay = max(payment, minpay)
        if base:
            npay = max(npay, bal / 24.0 + fee + interest)

        bal = nbal - npay
        payarray.append(npay)
        months -= 1

    return payarray


def annuity(pval, months, rate):
    ''' Calculates how much you have to pay each month if you want to
        pay exactly the same amount each month. The interesting input
        is the amount of $months.

        It does not include the fee so add that later.
    '''

    if months == 0:
        return pval

    if round(rate, 0) == 0:
        return pval / months
    p = rate / (100.0 * 12)
    return pval * p / (1 - pow(1 + p, -months))


def fixed(pval, monthly, rate, fromdayone):
    ''' How many months does it take to pay off a loan if I pay
        exactly monthly each month? It might actually go faster
        if you hit the minimum payments, but this function returns
        the longest amount of months.

        This function _does_ not include the fee, so remove the fee
        from the monthly before sending it into this function.

        Returns values: float months
                        int   -1        you are not paying more than
                                        the interest. infinity
                        int   -2        fromdayone has to be 0 or 1
    '''

    p = rate / (100 * 12)
    f = 1 + p

    if fromdayone == 0:
        if f < pval * p / monthly:
            return -1
        return 1 - log(f - pval * p / monthly) / log(f)
    elif fromdayone == 1:
        if 1.0 < pval * p / monthly:
            return -1
        return -log(1.0 - pval * p / monthly) / log(f)
    else:
        return -2


def apr_annuity(pval, months, rate, fee, minpay):
    ''' Calculate the APR for an annuity '''

    payment = annuity(pval, months, rate) + fee
    if payment < 0:
        return payment

    payarray = fulpacc(pval, rate, fee, minpay, payment, months, False)
    apr = irr2apr(irr(pval, payarray, 1))

    return apr


def apr_fixed(pval, payment, rate, fee, minpay):
    ''' Calculate the APR given a fixed payment each month. '''

    months = fixed(pval, payment - fee, rate, 1)
    if months < 0:
        return months

    months = ceil(months)
    payarray = fulpacc(pval, rate, fee, minpay, payment, months, False)
    apr = irr2apr(irr(pval, payarray, 1))

    return apr


def apr_min(pval, rate, fee, minpay):
    ''' This tries to pay the absolute minimum each month.
        Give the absolute worst APR.
        Don't export, only here for reference.
    '''

    payarray = fulpacc(pval, rate, fee, minpay, 0.0, -1, True)
    apr = irr2apr(irr(pval, payarray, 1))

    return apr


def apr_payin_X_months(pval, payment, rate, fee, minpay, free):
    ''' Calculates APR for a campaign where you give $free months to
        the client and there is no interest on the first invoice.
        The only new input is $free, and if you give "Pay in Jan"
        in November, then $free = 2.
    '''

    firstpay = payment
    months = fixed(pval, payment - fee, rate, 0)
    if months < 0:
        return months

    months = ceil(months)
    farray = [0.0 for f in range(free)]
    pval += fee

    farray.append(firstpay)
    pval -= firstpay

    payarray = fulpacc(pval, rate, fee, minpay, payment, months, False)
    farray.extend(payarray)
    apr = irr2apr(irr(pval, farray, 1))

    return apr


def get_payarr(tot, pclass, page):
    ''' Grabs the array of all monthly payments for specified PClass.
    '''

    if page is Page.CHECKOUT:
        monthsfee = pclass.invoicefee
        startfee = pclass.startfee
    else:
        monthsfee = 0
        startfee = 0

    # Include start fee in sum
    tot += startfee

    if pclass.type == PClass.Type.ACCOUNT:
        base = True
    else:
        base = False

    lowest = get_lowest_payment_for_account(pclass.country)
    if page is Page.CHECKOUT:
        minpay = lowest
    else:
        minpay = 0

    payment = annuity(tot, pclass.months, pclass.interestrate)
    payment += monthsfee

    return fulpacc(tot, pclass.interestrate, monthsfee, minpay, payment,
        pclass.months, base)


def calc_apr(tot, pclass, page, free=0):
    ''' Calculates APR for the specified values.
    '''

    tot = float(tot)

    if free < 0:
        raise KlarnaException("Number of free months must be positive or zero")

    if page is Page.CHECKOUT:
        monthsfee = pclass.invoicefee
        startfee = pclass.startfee
    else:
        monthsfee = 0
        startfee = 0

    # Include start fee in sum
    tot += startfee

    lowest = get_lowest_payment_for_account(pclass.country)
    if page is Page.CHECKOUT and pclass.type == PClass.Type.ACCOUNT:
        minpay = lowest
    else:
        minpay = 0

    payment = annuity(tot, pclass.months, pclass.interestrate) + monthsfee
    if pclass.type in (PClass.Type.CAMPAIGN, PClass.Type.ACCOUNT):
        apr = apr_annuity(tot, pclass.months, pclass.interestrate, pclass.invoicefee,
            minpay)
    elif pclass.type == PClass.Type.SPECIAL:
        apr = apr_payin_X_months(tot, payment, pclass.interestrate,
            pclass.invoicefee, minpay, free)
    elif pclass.type == PClass.Type.FIXED:
        apr = apr_fixed(tot, payment, pclass.interestrate, pclass.invoicefee, minpay)
    else:
        raise KlarnaException("Unknown PClass type %r" % pclass.type)

    return round(apr, 2)


def total_credit_purchase_cost(tot, pclass, page):
    ''' Calculates the total credit purchase cost.
    '''

    tot = float(tot)

    if page is Page.CHECKOUT:
        startfee = pclass.startfee
    else:
        startfee = 0

    payarray = get_payarr(tot, pclass, page)

    credit_cost = sum(payarray)
    return credit_cost + startfee


def calc_monthly_cost(tot, pclass, page):
    ''' Calculates the monthly cost for the specified pclass.
        The result is rounded up to the correct value depending on the pclasses
        country.
    '''

    payarray = get_payarr(tot, pclass, page)

    if len(payarray) > 0:
        val = payarray[0]
    else:
        val = 0

    if page is Page.CHECKOUT:
        return round(val, 2)

    return pround(val, pclass.country)


def get_lowest_payment_for_account(country):
    ''' Returns the lowest monthly payment for Klarna Account.
    '''

    lpby = {
        'SE': 50.0,
        'NO': 95.0,
        'FI': 8.95,
        'DK': 89.0,
        'DE': 6.95,
        'NL': 6.95}

    return lpby[country]


def pround(value, country):
    if country in ('DE', 'NL'):
        return round(value, 1)
    return round(value, 0)
''' Klarna API - Candice

Statistics reporting
'''

# Copyright 2011 KLARNA AB. All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#    1. Redistributions of source code must retain the above copyright notice,
#       this list of conditions and the following disclaimer.
#
#    2. Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY KLARNA AB "AS IS" AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
# EVENT SHALL KLARNA AB OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
# OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
# EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# The views and conclusions contained in the software and documentation are
# those of the authors and should not be interpreted as representing official
# policies, either expressed or implied, of KLARNA AB.

# python3k campatibility
import sys
if sys.version_info >= (3,):
    basestring = str

__all__ = ('Candice',)

import socket
import logging
from .digest import md5b64

logger = logging.getLogger('klarna')


class Candice(object):
    def __init__(self, address, port, eid, secret, url):
        self.__address = address
        self.__port = port
        self.__eid = eid
        self.__secret = secret
        self.__url = url

    def send_stat(self, method, time, select_time, status):
        vals = [str(self.__eid), method, str(time), str(select_time), str(status),
            self.__url]
        digest = md5b64('|'.join(vals + [self.__secret]))
        data = '|'.join(vals + [digest])

        logger.debug('candice %r', data)

        # Open connection to candice
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        except IOError:
            logger.warning("could not open candice socket", exc_info=True)

        # Send stats
        try:
            sock.sendto(data.encode(), (self.__address, self.__port))
        except:
            logger.warning('error sending candice report', exc_info=True)
        finally:
            sock.close()
# -*- coding: utf-8 -*-
''' Klarna API - Config

Defines a Config class that load and saves configuration as json
'''

# Copyright 2011 KLARNA AB. All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#    1. Redistributions of source code must retain the above copyright notice,
#       this list of conditions and the following disclaimer.
#
#    2. Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY KLARNA AB "AS IS" AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
# EVENT SHALL KLARNA AB OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
# OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
# EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# The views and conclusions contained in the software and documentation are
# those of the authors and should not be interpreted as representing official
# policies, either expressed or implied, of KLARNA AB.
# Defines the main API object class used to communicate with Klarna

# python3k campatibility
import sys
if sys.version_info >= (3,):
    basestring = str

__all__ = ('Config',)

import json
import logging

logger = logging.getLogger('klarna')


class Config(dict):
    ''' Configuration holder for the Klarna instance.

        backed by json on file if Config.store is set to True
    '''

    store = True

    def __init__(self, file=None, **kwargs):
        ''' if a single argument is given it's used as the filename to load
            the configuration from. Else the config is populated from the keyword
            arguments
        '''

        self.file = file

        # Load the file if any
        try:
            if self.file is not None:
                with open(self.file) as fp:
                    self.update(json.load(fp))
        except:
            logger.warn('Could not load config', exc_info=True)

        # Fill the config with values from keyword arguments
        self.update(kwargs)

    def __del__(self):
        try:
            if self.store and self.file is not None:
                self.save()
        except IOError:
            logger.warn('Could not save config', exc_info=True)

    def save(self):
        string = json.JSONEncoder().encode(self)
        with open(self.file, 'w') as fp:
            fp.write(string)
''' Klarna API - Constants

Provides constanst used throughout the API
'''

# Copyright 2011 KLARNA AB. All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#    1. Redistributions of source code must retain the above copyright notice,
#       this list of conditions and the following disclaimer.
#
#    2. Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY KLARNA AB "AS IS" AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
# EVENT SHALL KLARNA AB OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
# OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
# EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# The views and conclusions contained in the software and documentation are
# those of the authors and should not be interpreted as representing official
# policies, either expressed or implied, of KLARNA AB.
# Defines the main API object class used to communicate with Klarna


__reversed = {}


def lookup(mapping, value):
    try:
        # If the value can be converted to int, treat it as a constant
        # else try to use it as a code string
        value = int(value)
    except ValueError:
        value = value.upper()
        # Make sure the code is recognised and return as-is
        if value not in mapping:
            raise ValueError(value)
        return value
    else:
        # Get the reversed mapping
        try:
            r = __reversed[id(mapping)]
        except KeyError:
            r = dict([(v, k) for k, v in mapping.items()])
            __reversed[id(mapping)] = r

        try:
            return r[value]
        except KeyError:
            raise ValueError(value)

version = "2.3.1"


class Page(object):
    CHECKOUT = object()
    PRODUCT = object()


class Order(object):
    INVOICE = 0
    RESERVATION = 1


class ShipmentType(object):
    NORMAL = 1
    EXPRESS = 2


class GoodsIs(object):
    NOFLAG = 0
    PRINT1000 = 1
    PRINT100 = 2
    PRINT10 = 4
    SHIPPING = 8
    HANDLING = 16
    INC_VAT = 32


class Flag(object):
    NOFLAG = 0
    TEST_MODE = 2
    PCLASS_INVOICE = -1
    AUTO_ACTIVATE = 1
    PRE_PAY = 8  # Deprecated - About to be removed.
    INC_VAT = 32
    SENSITIVE_ORDER = 1024
    RETURN_OCR = 8192
    PHONE_TRANSACTION = 262144
    SEND_PHONE_PIN = 524288

    # Change reservation
    NEW_AMOUNT = 0
    ADD_AMOUNT = 1


class GetAddressFlag(object):
    ALL = 1
    LAST = 2
    GIVEN = 5


class Reserve(object):
    SEND_BY_MAIL = 1 << 2
    SEND_BY_EMAIL = 1 << 3
    PRESERVE = 1 << 4
    SENSITIVE_ORDER = 1 << 5
    PHONE_TRANSACTION = 1 << 9
    SEND_PHONE_PIN = 1 << 10


class OrderStatus(object):
    ACCEPTED = 1
    PENDING = 2
    DENIED = 3


class OrderStatusFor(int):
    pass


OrderStatusFor.INVOICE = OrderStatusFor(0)
OrderStatusFor.RESERVATION = OrderStatusFor(0)
OrderStatusFor.ORDERID = OrderStatusFor(1)


class Gender(object):
    FEMALE = 0
    MALE = 1


GenderMap = {
    'f': Gender.FEMALE,
    'm': Gender.MALE
}


class Fee(object):
    SHIPMENT = 1
    HANDLING = 2

CountryInfo = {
    'DE': ('EUR', 'DE'),
    'DK': ('DKK', 'DA'),
    'FI': ('EUR', 'FI'),
    'NL': ('EUR', 'NL'),
    'NO': ('NOK', 'NB'),
    'SE': ('SEK', 'SV'),
    'AT': ('EUR', 'DE')}

Countries = {
    'AT': 15,
    'DK': 59,
    'FI': 73,
    'DE': 81,
    'NL': 154,
    'NO': 164,
    'SE': 209}

Currencies = {
    'SEK': 0,
    'NOK': 1,
    'EUR': 2,
    'DKK': 3}

Languages = {
    'DA': 27,
    'DE': 28,
    'EN': 31,
    'FI': 37,
    'NB': 97,
    'NL': 101,
    'SV': 138}

# PNO/SSN encoding constants
Encoding = {
    'PNO_SE': 2,
    'PNO_NO': 3,
    'PNO_FI': 4,
    'PNO_DK': 5,
    'PNO_DE': 6,
    'PNO_NL': 7,
    'PNO_AT': 8,

    'CUSTNO': 1000,
    'EMAIL': 1001,
    'CELLNO': 1002,
    'BANK_BIC_ACC_NO': 1003}

Regexp = {
    'PNO_SE':
    r'^[0-9]{6,6}(([0-9]{2,2}[-\+]{1,1}[0-9]{4,4})|([-\+]{1,1}[0-9]{4,4})|'
    r'([0-9]{4,6}))$',

    'PNO_NO':
    r'^[0-9]{6,6}((-[0-9]{5,5})|([0-9]{2,2}((-[0-9]{5,5})|([0-9]{1,1})|'
    r'([0-9]{3,3})|([0-9]{5,5))))$',

    'PNO_FI':
    r'^[0-9]{6,6}(([A\+-]{1,1}[0-9]{3,3}[0-9A-FHJK-NPR-Y]{1,1})|([0-9]{3,3}'
    r'[0-9A-FHJK-NPR-Y]{1,1})|([0-9]{1,1}-{0,1}[0-9A-FHJK-NPR-Y]{1,1}))$',

    'PNO_DK':
    r'^[0-9]{8,8}([0-9]{2,2})?$',

    'PNO_DE':
    r'^[0-9]{7,9}$',

    'PNO_NL':
    r'^[0-9]{7,9}$',

    'PNO_AT':
    r'^[0-9]{7,9}$',

    'EMAIL':
    r'^[_a-zA-Z0-9-]+(\.[_a-zA-Z0-9-]+)*@[a-zA-Z0-9-]+(\.[a-zA-Z0-9-]+)*'
    r'(\.[a-zA-Z0-9-][a-zA-Z0-9-]+)+$',

    'CELLNO':
    r'(46|0)7[\ \-0-9]{8,13}$'}
''' Klarna API - Digest

helper for digest calculation done by candice and core api
'''

# Copyright 2011 KLARNA AB. All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#    1. Redistributions of source code must retain the above copyright notice,
#       this list of conditions and the following disclaimer.
#
#    2. Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY KLARNA AB "AS IS" AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
# EVENT SHALL KLARNA AB OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
# OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
# EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# The views and conclusions contained in the software and documentation are
# those of the authors and should not be interpreted as representing official
# policies, either expressed or implied, of KLARNA AB.

__all__ = ('get_digest',)

import sys
import hashlib
from base64 import b64encode


# Find a good hash algorithm
for m in ('sha512', 'sha256', 'md5'):
    try:
        default_method = getattr(hashlib, m)
        break
    except AttributeError:
        pass
else:
    raise Exception('Found no suitable hash algorithm')


# DEPRECATED
def md5b64(s):
    ''' Returns the md5 hash of s as a base64 encoded string (deprecated) '''
    return get_digest(s, hashlib.md5)


def get_digest(s, method=default_method):
    if sys.version_info >= (3,):
        s = s.encode('iso8859-1')
    else:
        if isinstance(s, unicode):
            s = s.encode('iso8859-1')
    return b64encode(method(s).digest()).decode('ascii')
# -*- coding: utf-8 -*-
''' Klarna API - Error

Defines exception subclasses used by Klarna
'''

# Copyright 2011 KLARNA AB. All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#    1. Redistributions of source code must retain the above copyright notice,
#       this list of conditions and the following disclaimer.
#
#    2. Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY KLARNA AB "AS IS" AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
# EVENT SHALL KLARNA AB OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
# OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
# EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# The views and conclusions contained in the software and documentation are
# those of the authors and should not be interpreted as representing official
# policies, either expressed or implied, of KLARNA AB.


class KlarnaException(Exception):
    ''' Exception class used for invalid API usage '''

    message = None
    code = 5000

    def __init__(self, message=None, code=None, root_cause=None):
        if message:
            self.message = message

        if code:
            self.code = code

        self.root_cause = root_cause

    def __str__(self):
        base = '(%d): %s' % (self.code, self.message)
        if self.root_cause is not None:
            return base + '[source %s: %s]' % (type(self.root_cause), self.root_cause)
        return base


class KlarnaValueException(KlarnaException):
    def __init__(self, value, message=None, code=None, root_cause=None):
        KlarnaException.__init__(self,
            message or self.message % value,
            code or self.code,
            root_cause)


class UnknownCountry(KlarnaValueException):
    message = "Unknown Country (%s)"
    code = 5002


class UnknownLanguage(KlarnaValueException):
    message = "Unknown Language (%s)"
    code = 5003


class UnknownCurrency(KlarnaValueException):
    message = "Unknown Currency (%s)"
    code = 5004


class UnknownFee(KlarnaValueException):
    message = "Unknown Fee type (%s)"
    code = 5102


class MissingHouseNumber(KlarnaException):
    message = "House number needs to be specified for Netherlands and Germany"
    code = 50014


class MissingCustomerNumber(KlarnaException):
    message = "customer number not set"
    code = 50050


class MissingArtNos(KlarnaException):
    message = "artNos not specified"
    code = 50063


class MissingGoodsList(KlarnaException):
    message = "Goods list not specified"
    code = 50038


class EmptyArtNos(KlarnaException):
    message = "Empty artNos sequence"
    code = 50064


class EmptyGoodsList(KlarnaException):
    message = 'No articles in goods list'
    code = 50038


class InvalidAmount(KlarnaException):
    message = "Amount needs to be larger than 0! (%s)"
    code = 5100


class MissingXmlRpc(KlarnaException):
    message = 'No XML-RPC object found, have you run Klarna.init()'
    code = 5101


class CurrencyLanguageMismatch(KlarnaException):
    message = "Mismatching currency/language for %s (%s, %s)"
    code = 50024

    def __init__(self, country, currency, language,
            message=None, code=None, root_cause=None):
        KlarnaException.__init__(self,
            message or self.message % (country, currency, language),
            code or self.code,
            root_cause)


class ShippingCountryMismatch(KlarnaException):
    message = "Shipping address country (%s) must match the country set (%s)"
    code = 50041

    def __init__(self, shipping_country, country,
            message=None, code=None, root_cause=None):
        KlarnaException.__init__(self,
            message or self.message % (shipping_country, country),
            code or self.code,
            root_cause)
# -*- coding: utf-8 -*-
''' Klarna API - ILT

Defines class to hold information about a ILT question
'''

# Copyright 2011 KLARNA AB. All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#    1. Redistributions of source code must retain the above copyright notice,
#       this list of conditions and the following disclaimer.
#
#    2. Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY KLARNA AB "AS IS" AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
# EVENT SHALL KLARNA AB OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
# OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
# EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# The views and conclusions contained in the software and documentation are
# those of the authors and should not be interpreted as representing official
# policies, either expressed or implied, of KLARNA AB.
# Defines the main API object class used to communicate with Klarna

# python3k campatibility
import sys
if sys.version_info >= (3,):
    basestring = str


class Info(object):
    def __init__(self, url):
        if sys.version_info >= (3,):
            from urllib.request import urlopen
        else:
            # Wrap urllib2's urlopen to support __exit__
            import contextlib
            import urllib2

            def urlopen(*args):
                return contextlib.closing(urllib2.urlopen(*args))

        import json
        with urlopen(url) as r:
            self._data = json.loads(r.read().decode())

    def get_questions(self, keys):
        def mkquestion(key, data):
            if 'values' in data:
                values = [Value(v['value'], v['text']) for v in data['values']]
            else:
                values = None

            return Question(key, data['text'],
                Question.Pre if data.get('pre', False) else Question.Ilt,
                data['type'],
                values
            )
        qdata = self._data['questions']
        return [mkquestion(key, qdata[key]) for key in keys]

    def get_dictionary(self):
        return self._data['dictionary']


class Value(object):
    ''' ILT Enumeration value '''

    def __init__(self, value, text):
        self.value = value
        self.text = text

    def __repr__(self):
        return self.value

    def __str__(self):
        return self.text


# pre_ilt - don't send to KO
class Question(object):
    ''' Holds information about a ILT question to ask the customer

        key: the name of the field to set in income_info with the answer
            to this question.
        text: the text of the question display to the customer
        answer_type: the type of input (enum, integer)
        answer_values: list of possible answers for the enum type
    '''

    Pre = object()
    Ilt = object()

    @property
    def is_select(self):
        return self.answer_type == 'enum'

    def __init__(self, key, text, question_type, answer_type, values):
        self.key = key
        self.text = text
        self.question_type = question_type
        self.answer_type = answer_type
        self.answer_values = values

    def __repr__(self):
        return 'Question(%s, %r, %r)' % (self.key,
            self.answer_type,
            self.answer_values)
#!/usr/bin/env python
# -*- coding: utf-8 -*-
''' Klarna API

Defines the main API object class used to communicate with Klarna
'''

# Copyright 2011 KLARNA AB. All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#    1. Redistributions of source code must retain the above copyright notice,
#       this list of conditions and the following disclaimer.
#
#    2. Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY KLARNA AB "AS IS" AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
# EVENT SHALL KLARNA AB OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
# OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
# EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# The views and conclusions contained in the software and documentation are
# those of the authors and should not be interpreted as representing official
# policies, either expressed or implied, of KLARNA AB.

# python3k campatibility

__all__ = ('Klarna',)

# System imports
import logging
import re
import time
from functools import partial

# Klarna submodules
from .util import *
from .const import *
from .error import *
from .addr import Address
from .pclass import PClass
from .candice import Candice
from .digest import get_digest
from . import pclasses, checkout

# logger shortcut
logger = logging.getLogger('klarna')


class ServerProxy(xmlrpc.ServerProxy):
    ''' Wrapper around xmlrpc ServerProxy that enables logging and statistics
        gathering of all calls.
    '''

    candice = None

    def __getattr__(self, key):
        method = xmlrpc.ServerProxy.__getattr__(self, key)

        def callwrapper(*args):
            status = 0
            start = time.time()
            logger.debug("calling %s(%s)",
                key, ', '.join([str(x) for x in args]))

            try:
                result = method(*args)

            except xmlrpc.Fault as e:
                # Grab the fault code in case it's going to be sent to candice
                status = e.faultCode
                raise e

            finally:
                end = time.time()
                # Send xml-rpc call stats to candice if requested
                if self.candice is not None:
                    duration = int(end - start)
                    self.candice.send_stat(key, duration, duration, status)

            logger.debug("[result] %s : %s", key, result)
            return result
        return callwrapper


def remote_call(fun):
    ''' Decorates the function fun with a check for the xmlrpc object '''
    def remote_call_wrapper(klarna, *args, **kwargs):
        if not hasattr(klarna, 'xmlrpc'):
            raise MissingXmlRpc()
        return fun(klarna, *args, **kwargs)
    remote_call_wrapper.__doc__ = fun.__doc__
    return remote_call_wrapper


def fix_amount(amount):
    return int(round(amount * 100))


def buildversion(version, ext):
    # build a version string like language:type:version:extra
    return ':'.join(['python', 'api', version, '-'.join(ext)]).strip(':')


def splitversion(v):
    # split package version into dotted version and extension
    vparts = v.split('-')
    return vparts[0], set(vparts[1:])


class Klarna(object):
    '''
        This API provides a way to integrate with Klarna's services over the
        XML-RPC protocol.

        For more information see
        http://integration.klarna.com/en/api/step-by-step
    '''

    PROTO = '4.1'
    _version, _ext = splitversion(version)
    VERSION = buildversion(_version, _ext)
    #ServerProxy = ServerProxy()

    ## Types used to tag arguments/return values with their use
    class OCR(Tag):
        ''' OCR Number '''
        pass

    class INVNO(Tag):
        ''' Invoice Number '''
        def amount(self):  # pragma: no cover
            return self.klarna.invoice_amount(self)

    class CUSTNO(Tag):
        ''' Customer Number '''
        pass

    class PNO(Tag):
        ''' Personal Number / Social security number '''
        pass

    class RNO(Tag):
        ''' Reservation number '''
        pass

    # The protocol to use when communicating with Klarna (http or https)
    scheme = 'https'

    # Iff the estore is using a proxy which populates the clients IP to
    # x_forwarded_for then this should be set to True
    use_x_forwarded_for = False

    # URL of the candice server
    c_addr = 'clientstat.klarna.com'

    # Port number to use for candice
    c_port = 80

    # Server definitions (url, default configuration)
    servers = {
        'live': ('payment.klarna.com', {'scheme': 'https'}),
        'beta': ('payment-beta.klarna.com', {'scheme': 'https'})
    }

    # default ports to use for http/https (standard RFC ports)
    default_ports = {
        'http': 80,
        'https': 443
    }

    # Default values for instances
    _client_addr = None
    _xfwd_addr = None
    pclasses = None
    co_objects = None

    @property
    def eid(self):
        ''' The estore's identifier received from Klarna '''
        if hasattr(self, '_eid'):
            return self._eid
        return self.config['eid']

    @eid.setter
    def eid(self, value):
        self._eid = int(value)

    @property
    def secret(self):
        ''' The estore's shared secret with klarna '''
        if hasattr(self, '_secret'):
            return self._secret
        return self.config['secret']

    @secret.setter
    def secret(self, value):
        self._secret = str(value)

    @property
    def country(self):
        ''' Country code
            code must be one of the keys in the Countries dictionary '''
        if hasattr(self, '_country'):
            return self._country
        return self.config['country']

    @country.setter
    def country(self, value):
        try:
            self._country = lookup(Countries, value)
        except ValueError:
            raise UnknownCountry(value)

    @property
    def language(self):
        ''' Language code
            code must be one of the keys in the Languages dictionary '''
        if hasattr(self, '_language'):
            return self._language
        return self.config['language']

    @language.setter
    def language(self, value):
        if value not in Languages:
            raise UnknownLanguage(value)
        self._language = value

    @property
    def currency(self):
        ''' Currency code
            code must be one of the keys in the Currencies dictionary '''
        if hasattr(self, '_currency'):
            return self._currency
        return self.config['currency']

    @currency.setter
    def currency(self, value):
        if value not in Currencies:
            raise UnknownCurrency("Unknown currency '%s'" % value)
        self._currency = value

    @property
    def billing(self):
        ''' Billing address used in the transaction
            shipping address will be used if not set '''
        if hasattr(self, '_billing'):
            return self._billing
        if not hasattr(self, '_shipping'):
            raise AttributeError("No Address set")
        return self._shipping

    @billing.setter
    def billing(self, value):
        logger.debug("Billing address %s", value)
        self._billing = value

    @property
    def shipping(self):
        ''' Shipping address used in the transaction
            billing address will be used if not set '''
        if hasattr(self, '_shipping'):
            return self._shipping
        if not hasattr(self, '_billing'):
            raise AttributeError("No Address set")
        return self._billing

    @shipping.setter
    def shipping(self, value):
        logger.debug("Shipping address %s", value)
        self._shipping = value

    @property
    def comment(self):
        ''' Comment string used in the transaction, the comment will be shown
            in the invoice.
        '''
        return '\n'.join(self._comments)

    @property
    def extrainfo(self):
        ''' Extra info used in the transaction

            Available named values are:
            * str - cust_no
            * str - estore_user
            * str - maiden_name
            * str - place_of_birth
            * str - password
            * str - new_password
            * str - captcha
            * int - poa_group
            * str - poa_pno
            * str - ready_date
            * str - rand_string
            * int - bclass
            * str - pin
        '''
        return self._extrainfo

    @property
    def activateinfo(self):
        ''' Activation Info that holds optional data used for the activate call

            Available named values are:
            int - bclass
            str - orderid1
            str - orderid2
            str - reference
            str - reference_code
            str - cust_no
        '''
        return self._activateinfo

    @property
    def bankinfo(self):
        ''' Bank info used in the transaction

            Available named values are:
            * int - bank_acc_bic
            * int - bank_acc_no
            * int - bank_acc_pin
            * int - bank_acc_tan
            * str - bank_name
            * str - bank_city
            * str - iban
        '''
        return self._bankinfo

    @property
    def incomeinfo(self):
        ''' Income info used in the transaction

            Available named values are:
            * int - yearly_salary
            * int - no_people_in_household
            * int - no_children_below_18
            * int - net_monthly_household_income
            * int - monthly_cost_accommodation
            * int - monthly_cost_other_loans
        '''
        return self._incomeinfo

    @property
    def shipinfo(self):
        ''' Shipment info used in the transaction

            Available named values are:
            * int - delay_adjust
            * str - shipping_company
            * str - shipping_product
            * dict - warehouse_addr
        '''
        return self._shipinfo

    @property
    def travelinfo(self):
        ''' Travel info used in the transaction

            Available named values are:
            * string - travel_company
            * string - reseller_company
            * string - departure_date
            * string - return_date
            * array  - destinations
            * array  - passenger_list
            * array  - passport_no
            * array  - driver_license_no
        '''
        return self._travelinfo

    @property
    def clientip(self):
        ''' IP address of the client
            populated by parse_wsgi_environment '''
        if self.use_x_forwarded_for and self._xfwd_addr is not None:
            return self._xfwd_addr
        if self._client_addr is None:
            raise AttributeError("clientip")
        return self._client_addr

    @clientip.setter
    def clientip(self, value):
        self._client_addr = value

    def __init__(self, config):
        ''' Creates the Klarna Object, to complete initialisation init() should
            also be called which will create the XML-RPC object

            config a klarna.Config object (or any other object implementing the
                mapping protocol) containing the configuration options to use

            required fields of the config object
            * eid
            * secret
            * pcstorage
            * pcuri
        '''

        self.config = config

        # Get XML-RPC server settings
        if 'uri' in config:
            self._uri = config['uri']
        else:
            addr, server_settings = self.servers[config.get('mode', 'beta')]
            scheme = server_settings.get('scheme', 'https')
            if 'port' in server_settings:
                port = server_settings['port']
            else:
                port = self.default_ports[self.scheme]

            # splice port number into uri
            addr = addr.split('/', 1)
            self._uri = '%s://%s:%s/%s' % (scheme, addr[0], port,
                addr[1] if len(addr) > 1 else '')

        # Should candice statistics collection be used?
        self.candice = config.get('candice', False)

        # PClass settings
        self.pcstorage = config['pcstorage']
        self.pcuri = config['pcuri']

        # Wrappers with a reference to this object
        self.OCR = partial(self.OCR, klarna=self)
        self.INVNO = partial(self.INVNO, klarna=self)
        self.CUSTNO = partial(self.CUSTNO, klarna=self)
        self.PNO = partial(self.PNO, klarna=self)
        self.RNO = partial(self.RNO, klarna=self)

        # Create empty containers
        self.clear()

    def init(self):
        ''' Initialises the XML-RPC object according to the configuration '''


        # Create ServerProxy object
        logger.info('Using XML-RPC server @ %s', self._uri)
        self.xmlrpc = self.ServerProxy(self._uri,
            verbose=self.config.get('dumpxml', False),
            encoding='iso-8859-1'
        )

        # Attach candice logging if requested
        if self.candice:
            self.xmlrpc.candice = Candice(self.c_addr, self.c_port,
                self.eid, self.secret, self._uri)

    def digest(self, *parts):
        return get_digest(kstr(':').join([str(x) for x in parts]))

    def __fetch_pclasses(self, storage, country, language, currency):
        # Calculate digest
        digest = self.digest(self.eid, Currencies[currency], self.secret)

        # Make the call
        result = self.xmlrpc.get_pclasses(self.PROTO, self.VERSION, self.eid,
            Currencies[currency], digest, Countries[country],
            Languages[language]
        )

        for pclass in result:
                pclass = PClass(
                    pclass[0],
                    pclass[1],
                    pclass[2],
                    float(pclass[3]) / 100,
                    float(pclass[4]) / 100,
                    float(pclass[5]) / 100,
                    float(pclass[6]) / 100,
                    pclass[7],
                    pclass[8],
                    pclass[9],
                    self.eid
                )
                storage.add_pclass(pclass)

    @remote_call
    def fetch_pclasses(self, country=None, language=None, currency=None):
        ''' Fetches the PClasses from Klarna, removes the cached/stored pclasses
            and updates.
            this method should only be called once!
        '''

        if country is None:
            country = self.country
        if language is None:
            language = self.language
        if currency is None:
            currency = self.currency

        # Save to local name to avoid having a broken pclass storage saved
        storage = self.get_pcstorage()

        # Attempt to load previously stored pclasses, so they aren't accidentially
        # removed
        try:
            storage.load(self.pcuri)
        except:
            logger.warning("Failed to load pclasses", exc_info=True)

        self.__fetch_pclasses(storage, country, language, currency)

        storage.save(self.pcuri)

        # Everyhing went fine, set storage on object
        self.pclasses = storage

    def get_pcstorage(self):
        storage = self.pcstorage
        if isinstance(storage, type) and issubclass(storage, pclasses.PCStorage):
            return storage()
        return pclasses.get_pclass_storage(storage)

    def clear_pclasses(self):
        ''' Removes the stored PClasses '''

        if not isinstance(self.pclasses, pclasses.PCStorage):
            self.pclasses = self.get_pcstorage()
        self.pclasses.clear(self.pcuri)
        self.pclasses = None

    def get_pclass(self, id):
        ''' Returns the specified PClass. '''

        check_type('PClass ID', id, (int,))

        # Load pclasses if needed
        if not isinstance(self.pclasses, pclasses.PCStorage):
            self.pclasses = self.get_pcstorage()
            self.pclasses.load(self.pcuri)

        # Query storage for pclass with given id and configured eid
        return self.pclasses.get_pclass(id, self.eid)

    def get_pclasses(self, type=None):
        ''' Retrieves the specified PClasses

            Type can be any of the values in klarna.PClass.Type
        '''

        # Load pclasses if needed
        if not isinstance(self.pclasses, pclasses.PCStorage):
            self.pclasses = self.get_pcstorage()
            self.pclasses.load(self.pcuri)

        pcls = self.pclasses.get_pclasses(self.country, type)
        pcls = list(pcls[self.eid].values())
        pcls.sort()
        return pcls

    @remote_call
    def check_ilt(self, pno, gender, amount, pclass=-1, encoding=None):
        ''' The purpose of this method is to check if the customer has
            answered the ILT questions. If the questions needs to be answered,
            an list will be returned with ILTQuestion objects.

            The answers should then be passed using set_income_info when
            calling reserve_amount/add_transaction. Using the key member
            of the question object as key and the answer from the customer as
            value.

            Note: You need to set shipping address before calling this method.

            To test this functionality with our test persons you can use these
            email-addresses to trigger the different possible answers
            * no_answer@ilt.klarna.com
                - not asked before
            * invalid_answer@ilt.klarna.com
                - old answers
            * valid_answer@ilt.klarna.com
                - nothing to ask

            amount: Amount including VAT
            pno: Personal number, SSN, date of birth, etc
            gender: Gender.MALE or Gender.FEMALE or None for unspecified
            encoding: Encoding constant for the PNO parameter.

        '''

        amount = fix_amount(amount)

        # Check pno syntax
        if encoding is None:
            encoding = self.get_pno_encoding()
        self.check_pno(pno, encoding)

        # Check gender
        if gender is None:
            gender = ''
        elif not isinstance(gender, int):
            gender = GenderMap[gender]

        # Check/Convert Addresses
        shipping = self.assemble_addr(self.shipping)

        # Make sure shipping country and set country match
        if shipping['country'] != Countries[self.country]:
            raise ShippingCountryMismatch(shipping['country'], self.country)

        # Calculate digest
        digest = self.digest(
            self.eid, pno + str(gender), pclass, amount, self.secret
        )

        # Make call
        result = self.xmlrpc.check_ilt(
            self.PROTO, self.VERSION,
            pno, gender, shipping, Currencies[self.currency],
            Countries[self.country], Languages[self.language],
            self.eid, digest, Encoding[encoding],
            pclass, int(amount)
        )

        return result

    @remote_call
    def get_addresses(self, pno, encoding=None, type=GetAddressFlag.GIVEN):
        ''' The get_addreses function is used to retrieve a customers adress(es).
            Using this, the customer is not required to enter any information only
            confirm the one presented to him/her.

            The get_adresses function can also be used for companies.
            If the customer enters a company number, it will return all the
            addresses where the company is registered at.

            The get_addresses function is ONLY allowed to be used for Swedish
            persons with the following conditions:
            * It can only be used if invoice or part payment is the default
                payment method.
            * It has to disappear if the customer chooses another payment method
            * The button is not allowed to be called "get address", but
                "continue" or it can be picked up automatically when all the
                numbers have been typed.

            http://integration.klarna.com/en/api/standard-integration/functions/\
getaddresses
        '''

        if self.country not in ('SE',):
            msg = "This method is only available for Swedish customers."
            raise KlarnaException(msg)

        # Check pno syntax
        if encoding is None:
            encoding = self.get_pno_encoding()
        self.check_pno(pno, encoding)

        # Calculate digest
        digest = self.digest(self.eid, pno, self.secret)

        # Make call
        result = self.xmlrpc.get_addresses(
            self.PROTO, self.VERSION, pno, self.eid,
            digest, Encoding[encoding], type, self.clientip
        )

        def build_addr(data):
            addr = Address()

            if type == GetAddressFlag.GIVEN:
                addr.is_company = True if len(data) == 5 else False
                if addr.is_company:
                    (addr.company, addr.street, addr.zip, addr.city,
                        addr.country
                    ) = data
                else:
                    (addr.fname, addr.lname, addr.street, addr.zip, addr.city,
                        addr.country
                    ) = data

            elif type == GetAddressFlag.LAST:
                # Here we cannot decide if it is a company or not? Assume private person.
                addr.is_company = False
                (addr.lname, addr.street, addr.zip, addr.city,
                    addr.country
                ) = data

            elif type == GetAddressFlag.ALL:
                if len(data[0]) > 0:
                    addr.fname, addr.lname = data[:2]
                else:
                    # Empty fname, assume it's a company
                    addr.is_company = True
                    addr.company = data[1]
                addr.street, addr.zip, addr.city, addr.country = data[2:]

            return addr

        # Create Addr objects from result set
        return [build_addr(data) for data in result]

    @remote_call
    def invoice_address(self, invno):
        ''' Retrieves the address used for an active invoice
            returns a Addr containing the address
        '''

        # Calculate digest
        digest = self.digest(self.eid, invno, self.secret)

        # Make call
        result = self.xmlrpc.invoice_address(
            self.PROTO, self.VERSION, self.eid,
            invno, digest
        )

        addr = Address()
        if len(result[0]) > 0:
            addr.is_company = False
            addr.fname, addr.lname = result[:2]
        else:
            # Empty fname, assume it's a company
            addr.is_company = True
            addr.company = result[1] # it's all empty anyway
        addr.street, addr.zip, addr.city, addr.country = result[2:]

        return addr

    @remote_call
    def invoice_amount(self, invno):
        ''' Retrieves the total amount for an active invoice.
            returns the total amount as float
        '''

        check_type('Invoice number', invno, (basestring,), Klarna.INVNO)

        # Calculate digest
        digest = self.digest(self.eid, invno, self.secret)

        # Make call
        result = self.xmlrpc.invoice_amount(
            self.PROTO, self.VERSION, self.eid,
            invno, digest
        )

        return result / 100.0

    @remote_call
    def update_orderno(self, invno, orderid):
        ''' Changes the order number of a purchase that was set when the order
            was made online.

            returns the invoice number
        '''

        check_type('Order number', orderid, (basestring,))
        check_type('Invoice number', invno, (basestring,), Klarna.INVNO)

        # Calculate digest
        digest = self.digest(invno, orderid, self.secret)

        # Make the call
        result = self.xmlrpc.update_orderno(
            self.PROTO, self.VERSION, self.eid,
            digest, invno, orderid
        )

        return self.INVNO(result)

    @remote_call
    def email_invoice(self, invno):
        ''' Sends an activated invoice to the customer via e-mail.
            The email is sent in plain text format and contains a link to a
            PDF-invoice.

            returns the invoice number
        '''

        # Calculate digest
        digest = self.digest(self.eid, invno, self.secret)

        # Make the call
        result = self.xmlrpc.email_invoice(
            self.PROTO, self.VERSION, self.eid,
            invno, digest
        )

        return self.INVNO(result)

    @remote_call
    def send_invoice(self, invno):
        ''' Requests a postal send-out of an activated invoice to a customer by
            Klarna (charges may apply)

            returns the invoice number
        '''

        # Calculate digest
        digest = self.digest(self.eid, invno, self.secret)

        # Make the call
        result = self.xmlrpc.send_invoice(
            self.PROTO, self.VERSION, self.eid,
            invno, digest
        )

        return self.INVNO(result)

    @remote_call
    def update_goods_qty(self, invno, artno, qty):
        ''' Changes the quantity of a specific item in a passive invoice.
            return the invoice number
        '''

        check_type('Invoice number', invno, (basestring,), Klarna.INVNO)
        check_type('Article Number', artno, (basestring,))
        check_type('Quantity', qty, (int,))

        # Calcualte digest
        digest = self.digest(invno, artno, qty, self.secret)

        # Make the call
        result = self.xmlrpc.update_goods_qty(
            self.PROTO, self.VERSION, self.eid,
            digest, invno, artno, qty
        )

        return self.INVNO(result)

    @remote_call
    def update_charge_amount(self, invno, type, amount):
        ''' Changes the amount of a fee (e.g. the invoice fee) in a passive
            invoice.

            returns the invoice number.
        '''

        check_type('Invoice number', invno, (basestring,), Klarna.INVNO)
        check_type('Amount', amount, (int, float))
        check_type('Fee type', type, (int,))
        amount = fix_amount(amount)

        # Calculate digest
        digest = self.digest(invno, type, amount, self.secret)

        # Make the call
        try:
            result = self.xmlrpc.update_charge_amount(
                self.PROTO, self.VERSION,
                self.eid, digest, invno, type, amount
            )
        except xmlrpc.ProtocolError as e:
            # re-raise as klarna exception
            # shadows any real http errors
            raise UnknownFee(type, root_cause=e)

        return self.INVNO(result)

    def add_article(self, qty=None, artno='', title='', price=None, vat=None,
                    discount=0, flags=Flag.INC_VAT, **kwargs):
        ''' Adds an article to the current goods list for the current order.

            Note: It is recommended that you use GoodIs.INC_VAT in flags

            Flags can be:
            GoodsIs.INC_VAT
            GoodsIs.SHIPPING
            GoodsIs.HANDLNIG
            GoodsIs.PRINT1000
            GoodsIs.PRINT100
            GoodsIs.PRINT10
        '''

        # Verify input
        check_type('Quantity', qty, (int,))
        check_type('Price', price, (int, float))
        check_type('VAT', vat, (int, float))
        check_type('Discount', discount, (int, float))

        if not artno and not title:
            raise KlarnaException("Title or artNo needs to be set")

        # Append article dictionary to goods list
        if not hasattr(self, 'goodslist'):
            self.goodslist = []

        article = {
            'artno': kstr(artno),
            'title': kstr(title),
            'price': fix_amount(price),
            'vat': vat,
            'discount': discount,
            'flags': flags}

        self.goodslist.append({'goods': article, 'qty': qty})
        logger.debug("article added %s", article)

    def add_art_no(self, qty=None, artno=None, **kwargs):
        ''' adds an article number and quantity to be used in activate_part,
            credit_part or invoice_part_amount
        '''

        check_type('Quantity', qty, (int,))

        if not hasattr(self, 'artnos'):
            self.artnos = []

        self.artnos.append({'artno': kstr(artno), 'qty': qty})

    @remote_call
    def add_transaction(self, pno, gender, flags=0, pclass=-1, encoding=None,
                        clear=True):
        ''' Assemble and send the current order to Klarna
            This clears all order data unless clear is set to False.

            returns a tuple with invoice number and Order status flag or raises
            a xmlrpc.Fault exception.

            If the flag Flag.RETURN_OCR is used the returned tuple will be
            (Invoice Number, OCR Number, Order Status flag)

            Gender is only required for Germany and Netherlands.

            Flags can be:
            * Flag.TEST_MODE
            * Flag.AUTO_ACTIVATE
            * Flag.SENSITIVE_ORDER
            * Flag.RETURN_OCR
            * Flag.MOBILEPHONE_TRANSACTION
            * Flag.SEND_PHONE_PIN

            Note:
            Normal shipment is assumed unless otherwise specified, you can do
            this by calling set_shipment_info(delay_adjust=...) with any
            value from ShipmentType

            http://integration.klarna.com/en/api/standard-integration/functions/\
addtransaction
        '''

        check_type('PNO/SSN', pno, (basestring,), Klarna.PNO)

        if gender is None:
            gender = ''

        # Check pno syntax
        if encoding is None:
            encoding = self.get_pno_encoding()
        self.check_pno(pno, encoding)

        # Check that there is articles for this transaction
        if not hasattr(self, 'goodslist'):
            raise MissingGoodsList()
        if len(self.goodslist) < 1:
            raise EmptyGoodsList()

        # Check/Convert Addresses
        billing = self.assemble_addr(self.billing)
        shipping = self.assemble_addr(self.shipping)

        # Assume normal shipment unless otherwise specified
        if 'delay_adjust' not in self.shipinfo:
            self.shipinfo['delay_adjust'] = ShipmentType.NORMAL

        # Make sure shipping country and set country match
        if shipping['country'] != Countries[self.country]:
            raise ShippingCountryMismatch(shipping['country'], self.country)

        # Calculate digest
        digest = self.digest(
            *([goods['goods']['title'] for goods in self.goodslist] +
                [self.secret]
            )
        )

        # Make call
        result = self.xmlrpc.add_invoice(
            self.PROTO, self.VERSION,
            pno,
            gender,
            self.reference,
            self.reference_code,
            self.orderid[0],
            self.orderid[1],
            shipping,
            billing,
            self.clientip,
            flags,
            Currencies[self.currency],
            Countries[self.country],
            Languages[self.language],
            self.eid,
            digest,
            Encoding[encoding],
            pclass,
            self.goodslist,
            self.comment,
            self.shipinfo,
            self.travelinfo,
            self.incomeinfo,
            self.bankinfo,
            self.sid,
            self.extrainfo
        )

        if clear:
            self.clear()

        return result

    @remote_call
    def has_account(self, pno, encoding=None):
        ''' Checks if the specified SSN/PNO has an part payment account with Klarna.

            http://integration.klarna.com/en/api/standard-integration/functions/\
hasaccount
        '''

        check_type('PNO/SSN', pno, (basestring,), Klarna.PNO)

        # Check pno syntax
        if encoding is None:
            encoding = self.get_pno_encoding()
        self.check_pno(pno, encoding)

        # Calculate digest
        digest = self.digest(self.eid, pno, self.secret)

        # Make the call
        result = self.xmlrpc.has_account(self.PROTO, self.VERSION, self.eid, pno,
            digest, Encoding[encoding])

        # Convert result to proper python type
        return {'true': True, 'false': False}[result]

    @remote_call
    def check_order_status(self, id, type=None):
        ''' Returns the current order status for a specific reservation or invoice
            Use this when add_transaction or reserve_amount returns OrderStatus.PENDING

            Order status can be:
            OrderStatus.ACCEPTED
            OrderStatus.PENDING
            OrderStatus.DENIED

            http://integration.klarna.com/en/api/other-functions/functions/\
checkorderstatus
        '''

        if type is not None:
            if type is OrderStatusFor.INVOICE:
                check_type('Invoice Number', id, (basestring,), Klarna.INVNO)
            elif type is OrderStatusFor.RESERVATION:
                check_type('Reservation Number', id, (basestring,), Klarna.RNO)
        else:
            if isinstance(id, Klarna.INVNO):
                type = OrderStatusFor.INVOICE
            elif isinstance(id, Klarna.RNO):
                type = OrderStatusFor.RESERVATION
            else:
                # Default to invoice
                type = OrderStatusFor.INVOICE

        # Calculate digest
        digest = self.digest(self.eid, id, self.secret)

        # Make the call
        result = self.xmlrpc.check_order_status(self.PROTO, self.VERSION, self.eid,
            digest, id, int(type))

        return result

    @remote_call
    def activate_invoice(self, invno, pclass=-1, clear=True):
        ''' Activates previously created invoice by add_transaction

            Note:
            If you want to change shipment type, you can specify it using
            set_shipment_info(delay_adjust=...)
            with either value from ShipmentType

            returns a url to PDF of the activated invoice

            http://integration.klarna.com/en/api/standard-integration/functions/\
activateinvoice
        '''

        check_type('Invoice number', invno, (basestring,), Klarna.INVNO)

        # Assume normal shipment unless otherwise specified
        if 'delay_adjust' not in self.shipinfo:
            self.shipinfo['delay_adjust'] = ShipmentType.NORMAL

        # Calculate digest
        digest = self.digest(self.eid, invno, self.secret)

        # Make call
        result = self.xmlrpc.activate_invoice(self.PROTO, self.VERSION, self.eid,
            invno, digest, pclass, self.shipinfo)

        if clear:
            self.clear()

        return result

    @remote_call
    def delete_invoice(self, invno):
        ''' Removes a passive invoices which has previously been created by
            add_transaction,

            Returns True if the invoice was successfully removed, otherwise
            an xmlrpc.Fault is raised.
        '''

        check_type('Invoice number', invno, (basestring,), Klarna.INVNO)

        # Calculate digest
        digest = self.digest(self.eid, invno, self.secret)

        # Make call
        result = self.xmlrpc.delete_invoice(self.PROTO, self.VERSION, self.eid,
            invno, digest)

        return result == 'ok'

    @remote_call
    def reserve_amount(self, pno, gender, amount=None, flags=0, pclass=-1,
                       encoding=None, clear=True):
        ''' Reserves a purchase amount for a specific customer.
            the reservation is valid, by default, for 7 days.

            Returns a tuple with reservation number and order status flag or
            a xmlrpc.Fault is raised.

            Note:
            Activation must be done with activate_reservation, you can not
            activate through Klarna Online.

            Gender is only required for Germany and the Netherlands.

            pno: Personal number, SSN, date of birth etc
            gender: Gender.MALE, Gender.FEMALE or None
            amount: The amount to be reserved
            flags: Options which affect the behaviour
            pclass: pclass name
            encoding: PNO encoding
            clear: Whether customer info should be cleared after this call

            Flags can be set to:
            * Flag.TEST_MODE
            * Reserve.SENSITIVE_ORDER
            * Reserve.PHONE_TRANSACTION
            * Reserve.SEND_PHONE_PIN

            http://integration.klarna.com/en/api/advanced-integration/functions/\
reserveamount
        '''

        if gender is None:
            gender = ''

        country = self.country
        language = self.language
        currency = self.currency

        check_type('PNO/SSN', pno, (basestring,), Klarna.PNO)
        check_type('Flags', flags, (int,))

        if amount == None:

            if not hasattr(self, 'goodslist'):
                raise MissingGoodsList()
            if len(self.goodslist) < 1:
                raise EmptyGoodsList()
            # Calculate amount from goodslist
            amount = int(self.summarize_goodslist())
        else:
            check_type('Amount', amount, (int, float))
            amount = fix_amount(amount)

        if amount <= 0:
            raise InvalidAmount(amount)

        # Check pno syntax
        if encoding is None:
            encoding = self.get_pno_encoding()
        self.check_pno(pno, encoding)

        # No addresses for phone transactions
        if flags & Reserve.PHONE_TRANSACTION:
            billing = shipping = ''

        # Check/Convert Addresses
        else:
            billing = self.assemble_addr(self.billing)
            shipping = self.assemble_addr(self.shipping)

            # Make sure shipping country and set country match
            if shipping['country'] != Countries[self.country]:
                raise ShippingCountryMismatch(shipping['country'], self.country)

        # Assume normal shipment unless otherwise specified
        if 'delay_adjust' not in self.shipinfo:
            self.shipinfo['delay_adjust'] = ShipmentType.NORMAL

        # Calculate digest
        digest = self.digest(self.eid, pno, amount, self.secret)

        # Make call
        result = self.xmlrpc.reserve_amount(
            self.PROTO, self.VERSION, pno, gender,
            amount, self.reference, self.reference_code, self.orderid[0],
            self.orderid[1], shipping, billing, self.clientip, flags,
            Currencies[self.currency], Countries[self.country],
            Languages[self.language], self.eid, digest, Encoding[encoding],
            pclass, getattr(self, 'goodslist', []), self.comment, self.shipinfo,
            self.travelinfo, self.incomeinfo, self.bankinfo, self.sid,
            self.extrainfo
        )

        if clear:
            self.clear()

        return result

    @remote_call
    def cancel_reservation(self, rno):
        ''' Cancels a reservation
            returns True if the reservation was successfully canceled else
            raises a xmlrpc.Fault

            rno: Reservation number

            http://integration.klarna.com/en/api/advanced-integration/functions/\
cancelreservation
        '''

        check_type('Reservation number', rno, (basestring,), Klarna.RNO)

        # Calculate digest
        digest = self.digest(self.eid, rno, self.secret)

        # Make call
        result = self.xmlrpc.cancel_reservation(
            self.PROTO, self.VERSION, rno,
            self.eid, digest
        )

        return result == 'ok'

    @remote_call
    def change_reservation(self, rno, amount, flags=Flag.NEW_AMOUNT):
        ''' Changes the specified reservation to a new amount
            returns True

            rno:    Reservation number
            amount: Amount including VAT
            flags:  Options which affect the behaviour

            Flags can be set to:
            Flag.NEW_AMOUNT
            Flag.ADD_AMOUNT

            http://integration.klarna.com/en/api/advanced-integration/functions/\
changereservation
        '''

        check_type('Reservation number', rno, (basestring,), Klarna.RNO)
        check_type('Amount', amount, (int, float))
        check_type('Flags', flags, (int,))

        amount = fix_amount(amount)

        # Calculate digest
        digest = self.digest(self.eid, rno, amount, self.secret)

        # Make call
        result = self.xmlrpc.change_reservation(
            self.PROTO, self.VERSION, rno,
            amount, self.eid, digest, flags
        )

        return result == 'ok'

    @remote_call
    def update(self, rno, clear=True):
        ''' Update the reservation matching the given reservation number.

            Uses information from set billing and shipping address to update
            addresses, goodslist to update goodslist, and set_estore_info to
            update orderid1 and orderid2.

            returns True if the reservation was successfully updated.

            rno:   Reservation number
            clear: clear set data after updating. Defaulted to True.
        '''
        check_type('Reservation number', rno, (basestring,), Klarna.RNO)

        # Start the digest hash
        digest_array = [
            self.PROTO.replace(".", ":"),
            self.VERSION,
            self.eid,
            rno
        ]

        # We only need parts of the addresses in the digest, in a very
        # specific order.
        shipping = []
        billing = []
        if (hasattr(self, 'shipping')):
            shipping = self.assemble_addr(self.shipping)
            digest_array += self.get_update_digest_parts(shipping)
        if (hasattr(self, 'billing')):
            billing = self.assemble_addr(self.billing)
            digest_array += self.get_update_digest_parts(billing)

        # If there are any items in the goodslist, add them to the digest
        if (hasattr(self, 'goodslist')):
            for goods in self.goodslist:
                # if artno is not set we have to use title for the digest.
                if len(goods['goods']['artno']) > 0:
                    digest_array.append(goods['goods']['artno'])
                else:
                    digest_array.append(goods['goods']['title'])
                # add quantity to the digest
                digest_array.append(goods['qty'])
        else:
            self.goodslist = []
        # if orderids are set, add them to the digest
        if (len(self.orderid[0])):
            digest_array.append(self.orderid[0])
        if (len(self.orderid[1])):
            digest_array.append(self.orderid[1])

        # finalize the digest by adding the secret and hashing it
        digest_array.append(self.secret)
        digest = self.digest(*(digest_array))

        result = self.xmlrpc.update(
            self.PROTO,
            self.VERSION,
            self.eid,
            digest,
            rno,
            {
                "goods_list": self.goodslist,
                "dlv_addr": shipping,
                "bill_addr": billing,
                "orderid1": self.orderid[0],
                "orderid2": self.orderid[1]
            })

        if clear:
            self.clear()

        return result == 'ok'


    @remote_call
    def activate(self, rno, ocr='', flags=0, clear=True):
        ''' Activate the reservation matching the given reservation number.
            Optional information should be set in ActivateInfo.

            To perform a partial activation, use the addArtNo function to specify
            which items in the reservation to include in the activation.

            returns a tuple with:
                Risk status ("no_risk", "ok")
                Invoice number

            rno:   Reservation number
            ocr:   optional OCR number to attach to the reservation when\
activating.
            flags: optional flags to affect behavior.
            clear: clear set data after activating. Defaulted to true.
        '''
        check_type('Reservation number', rno, (basestring,), Klarna.RNO)

        if ocr:
            self.set_activate_info(ocr=ocr)

        if flags:
            self.set_activate_info(flags=flags)

        # Assume normal shipment unless otherwise specified
        if 'delay_adjust' not in self.shipinfo:
            self.set_shipment_info(delay_adjust=ShipmentType.NORMAL)

        self.set_activate_info(shipment_info=self.shipinfo)

        # Create digest
        digest_array = [
            self.PROTO.replace(".", ":"),
            self.VERSION,
            self.eid,
            rno
        ]
        optional_keys = [
            "bclass", "cust_no", "flags", "ocr", "orderid1", "orderid2",
            "reference", "reference_code"
        ]
        for item in optional_keys:
            if item in self.activateinfo:
                digest_array.append(self.activateinfo[item])

        digest_array.append(self.activateinfo['shipment_info']['delay_adjust'])

        if hasattr(self, 'artnos'):
            for artno in self.artnos:
                digest_array.append(artno['artno'])
                digest_array.append(artno['qty'])
            self.set_activate_info(artnos=self.artnos)

        digest_array.append(self.secret)
        digest = self.digest(*(digest_array))
        # End of Digest

        # Send the call to Klarna
        result = self.xmlrpc.activate(
            self.PROTO,
            self.VERSION,
            self.eid,
            digest,
            rno,
            self.activateinfo
        )

        if clear:
            self.clear()

        risk, invno = result

        return (risk, self.INVNO(invno))


    @remote_call
    def activate_reservation(self, pno, rno, gender, ocr='', flags=0, pclass=-1,
                             encoding=None, clear=True):
        ''' Activates a previously created reservation.

            returns a tuple with:
                Risk status ("no_risk", "ok")
                Invoice number

            Gender is only required for Germany and the Netherlands.

            Note:
            Normal shipment is assumed unless otherwise specified, you can do
            this by calling set_shipment_info(delay_adjust=...) with any value
            from ShipmentType

            pno:        Personal number, SSN, date of birth, etc.
            rno:        Reservation number.
            gender:     'MALE, 'FEMALE' or None
            ocr:        A OCR number.
            flags:      Options which affect the behaviour.
            pclass:     pclass name
            encoding:   PNO encoding
            clear:      Whether customer info should be cleared after this call.

            Flags can be set to:
            Flag.TEST_MODE
            Reserve.SEND_BY_MAIL
            Reserve.SEND_BY_EMAIL
            Reserve.PRESERVE_RESERVATION
            Reserve.SENSITIVE_ORDER

            http://integration.klarna.com/en/api/advanced-integration/functions/\
activatereservation
        '''

        if gender is None:
            gender = ''

        country = self.country
        language = self.language
        currency = self.currency

        check_type('PNO/SSN', pno, (basestring,), Klarna.PNO)
        check_type('Reservation number', rno, (basestring,), Klarna.RNO)
        check_type('Flags', flags, (int,))

        if not hasattr(self, 'goodslist'):
            raise MissingGoodsList()
        if len(self.goodslist) < 1:
            raise EmptyGoodsList()

        # Check pno syntax
        if encoding is None:
            encoding = self.get_pno_encoding()
        self.check_pno(pno, encoding)

        # Check/Convert Addresses
        billing = self.assemble_addr(self.billing)
        shipping = self.assemble_addr(self.shipping)

        # Assume normal shipment unless otherwise specified
        if 'delay_adjust' not in self.shipinfo:
            self.shipinfo['delay_adjust'] = ShipmentType.NORMAL

        # Make sure shipping country and set country match
        if shipping['country'] != Countries[self.country]:
            raise ShippingCountryMismatch(shipping['country'], self.country)

        # Calculate digest
        digest = self.digest(
            *(
                [self.eid, pno] +
                [
                    '%s:%s' % (g['goods']['artno'], g['qty'])
                        for g in self.goodslist
                ] +
                [self.secret]
            )
        )

        # Make call
        result = self.xmlrpc.activate_reservation(
            self.PROTO, self.VERSION,
            rno,
            ocr,
            pno,
            gender,
            self.reference,
            self.reference_code,
            self.orderid[0],
            self.orderid[1],
            shipping,
            billing,
            "0.0.0.0",
            flags,
            Currencies[self.currency],
            Countries[self.country],
            Languages[self.language],
            self.eid,
            digest,
            Encoding[encoding],
            pclass,
            self.goodslist,
            self.comment,
            self.shipinfo,
            self.travelinfo,
            self.incomeinfo,
            self.bankinfo,
            self.extrainfo
        )

        if clear:
            self.clear()

        risk, invno = result
        return (risk, self.INVNO(invno))

    @remote_call
    def split_reservation(self, rno, amount, flags=0):
        ''' Splits a reservation due to for example outstanding articles.
            returns the new reservation number.

            For flags usage see reserve_amount

            rno:        Reservation number
            amount:     The amount to be subtracted from the reservation.
            flags:      Options which affect the behaviour.

            http://integration.klarna.com/en/api/advanced-integration/functions/\
splitreservation
        '''
        check_type('Reservation number', rno, (basestring,), Klarna.RNO)
        check_type('Amount', amount, (int, float))
        check_type('Flags', flags, (int,))

        amount = fix_amount(amount)
        if amount <= 0:
            raise InvalidAmount(amount)

        # Calculate digest
        digest = self.digest(self.eid, rno, amount, self.secret)

        # Make call
        rno, status = self.xmlrpc.split_reservation(
            self.PROTO, self.VERSION,
            rno, amount, self.orderid[0], self.orderid[1], flags, self.eid,
            digest
        )

        return self.RNO(rno), status

    @remote_call
    def activate_part(self, invno, pclass=-1, clear=True):
        ''' Partially activates a passive invoice

            returns a dictionary with ('url') or ('url' and 'invno')
            the value of url points to a pdf-version of the activated invoice.
            the value of invno is the number on the new passive invoice

            Note:
            You need to call add_artno first, to specify which articles and how
            many you want to activate.

            If you want to change shipment type, you can specify it using
            set_shipment_info(delay_adjust=...)
            with either value from ShipmentType
        '''

        check_type('Invoice number', invno, (basestring,), Klarna.INVNO)

        if not hasattr(self, 'artnos'):
            raise MissingArtNos()
        if len(self.artnos) < 1:
            raise EmptyArtNos()

        # Assume normal shipment unless otherwise specified
        if 'delay_adjust' not in self.shipinfo:
            self.shipinfo['delay_adjust'] = ShipmentType.NORMAL

        # Calculate digest
        digest = self.digest(*([self.eid, invno] +
            ['%s:%s' % (artno['artno'], artno['qty']) for artno in self.artnos] +
            [self.secret])
        )

        # Make call
        result = self.xmlrpc.activate_part(
            self.PROTO, self.VERSION,
            self.eid, invno, self.artnos, digest, pclass, self.shipinfo
        )

        if clear:
            self.clear()

        if 'invno' in result:
            result['invno'] = self.INVNO(result['invno'])
        return result

    @remote_call
    def credit_part(self, invno, credno=''):
        ''' Performs a partial refund on an invoice, part payment or mobile
            purchase.
            returns invoice number

            Note:
            You need to call add_artno first

            http://integration.klarna.com/en/api/invoice-handling-functions/functions/\
creditpart
        '''

        check_type('Invoice number', invno, (basestring,), Klarna.INVNO)
        check_type('Credit number', credno, (basestring, int))

        if not hasattr(self, 'artnos'):
            raise MissingArtNos()
        if len(self.artnos) < 1:
            raise EmptyArtNos()

        # Calculate digest
        digest = self.digest(
            *(
                [self.eid, invno] +
                [
                    '%s:%s' % (artno['artno'], artno['qty'])
                        for artno in self.artnos
                ] +
                [self.secret]
            )
        )

        # Make call
        result = self.xmlrpc.credit_part(
            self.PROTO, self.VERSION, self.eid, invno, self.artnos, credno,
            digest
        )

        return self.INVNO(result)

    @remote_call
    def return_amount(self, invno, amount, vat, flags=Flag.INC_VAT, description=""):
        ''' Gives discounts on invoicers
            returns invoice number

            If you are using standard integration and the purchase is not yet activated
            (you have not yet delivered the goods), just change the article list in our
            online interface Klarna Online.

            Flags can be:
            GoodsIs.INC_VAT

            invno: Invoice number
            amount: The amount given as a discount
            vat: VAT percent as float
            description: Optional text to replace the default "Discount" text on\
the customers invoice.

            http://integration.klarna.com/en/api/invoice-handling-functions/functions/\
returnamount
        '''

        check_type('Invoice number', invno, (basestring,), Klarna.INVNO)
        check_type('Amount', amount, (int, float))
        check_type('VAT', vat, (int, float))
        check_type('Flags', flags, (int,))

        amount = fix_amount(amount)
        #if amount <= 0:
        #   raise KlarnaException("Amount needs to be larger than 0! (%s)" % amount)

        # Calculate digest
        digest = self.digest(self.eid, invno, self.secret)

        # Make the call
        result = self.xmlrpc.return_amount(
            self.PROTO, self.VERSION,
            self.eid, invno, amount, vat, digest, flags, description
        )

        return self.INVNO(result)

    @remote_call
    def credit_invoice(self, invno, credno=''):
        ''' Performs a complete refund on an invoice, part payment and mobile
            purchase.

            returns the invoice number

            invno: Invoice Number
            credno: Credit number

            http://integration.klarna.com/en/api/invoice-handling-functions/functions/\
creditinvoice
        '''

        check_type('Invoice number', invno, (basestring,), Klarna.INVNO)
        check_type('credno', credno, (basestring,))

        # Calculate digest
        digest = self.digest(self.eid, invno, self.secret)

        # Make the call
        result = self.xmlrpc.credit_invoice(
            self.PROTO, self.VERSION, self.eid, invno, credno, digest
        )

        return self.INVNO(result)

    @remote_call
    def invoice_part_amount(self, invno):
        ''' Retrieves the amount of a specific goods from a purchase.

            Note:
            You need to call add_artno first.

            invno: Invoice number

            http://integration.klarna.com/en/api/other-functions/functions/\
invoicepartamount
        '''

        check_type('Invoice number', invno, (basestring,), Klarna.INVNO)

        if not invno:
            raise KlarnaException('Invoice number not set')

        if not hasattr(self, 'artnos'):
            raise MissingArtNos()
        if len(self.artnos) < 1:
            raise EmptyArtNos()

        # Calculate digest
        digest = self.digest(
            *(
                [self.eid, invno] +
                [
                    '%s:%s' % (artno['artno'], artno['qty'])
                        for artno in self.artnos
                ] +
                [self.secret]
            )
        )

        # Make call
        result = self.xmlrpc.invoice_part_amount(
            self.PROTO, self.VERSION,
            self.eid, invno, self.artnos, digest
        )

        return result / 100.0

    @remote_call
    def reserve_OCR(self, count, country=None):
        ''' Reserves a specified number of OCR numbers for the specified country
            or the configured country of this object.

            returns a list of OCR numbers.

            count: The number of OCR numbers to reserve
            country: country code of country to reserve OCR numbers for

            http://integration.klarna.com/en/api/advanced-integration/functions/\
reserveocrnums
        '''

        check_type('Count', count, (int,))

        if country is None:
            country = self.country

        # Calculate digest
        digest = self.digest(self.eid, count, self.secret)

        # Make the call
        result = self.xmlrpc.reserve_ocr_nums(
            self.PROTO, self.VERSION,
            count, self.eid, digest, Countries[country]
        )

        return [self.OCR(s) for s in result]

    @remote_call
    def reserve_OCR_email(self, count, email, country=None):
        ''' Reserves the number of OCRs specified and sends them to the given email.
            returns True if the OCRs were created and sent.
        '''

        check_type('Count', count, (int,))
        self.check_email(email)

        if country is None:
            country = self.country

        # Calculate digest
        digest = self.digest(self.eid, count, self.secret)

        # Make the call
        result = self.xmlrpc.reserve_ocr_nums_email(
            self.PROTO, self.VERSION,
            count, email, self.eid, digest, Countries[country]
        )

        return result == 'ok'

    @remote_call
    def get_customer_no(self, pno, encoding=None):
        ''' Retrieves a list of all the customer numbers associated with the
            specified pno.

            returns list containing all customer numbers associated with that pno.
        '''

        check_type('PNO/SSN', pno, (basestring,), Klarna.PNO)

        # Check pno syntax
        if encoding is None:
            encoding = self.get_pno_encoding()
        self.check_pno(pno, encoding)

        # Calculate digest
        digest = self.digest(self.eid, pno, self.secret)

        # Make the call
        result = self.xmlrpc.get_customer_no(
            self.PROTO, self.VERSION,
            pno, self.eid, digest, Encoding[encoding]
        )

        return [self.CUSTNO(r) for r in result]

    @remote_call
    def set_customer_no(self, pno, custno, encoding=None):
        ''' Associates a pno with a customer number when you want to make future
            purchases without a pno.

            returns True
        '''

        check_type('PNO/SSN', pno, (basestring,), Klarna.PNO)

        # Check pno syntax
        if encoding is None:
            encoding = self.get_pno_encoding()
        self.check_pno(pno, encoding)

        if not custno:
            raise MissingCustomerNumber()

        # Calculate digest
        digest = self.digest(self.eid, pno, custno, self.secret)

        # Make the call
        result = self.xmlrpc.set_customer_no(
            self.PROTO, self.VERSION,
            pno, custno, self.eid, digest, Encoding[encoding]
        )

        return result == 'ok'

    @remote_call
    def remove_customer_no(self, custno):
        ''' Removes a customer number from association with a pno.
            returns True
        '''

        if not custno:
            raise MissingCustomerNumber()

        # Calculate digest
        digest = self.digest(self.eid, custno, self.secret)

        # Make the call
        result = self.xmlrpc.remove_customer_no(self.PROTO, self.VERSION, custno,
            eid, digest)

        return result == 'ok'

    @remote_call
    def update_notes(self, invno, notes):
        ''' Sets notes/log information for the specified invoice number.
            returns Invoice number
        '''

        check_type('Invoice number', invno, (basestring,), Klarna.INVNO)

        # Calculate digest
        digest = self.digest(invno, notes, self.secret)

        # Make the call
        result = self.xmlrpc.update_notes(
            self.PROTO, self.VERSION,
            self.eid, digest, invno, notes
        )

        return self.INVNO(result)

    def get_pno_encoding(self):
        ''' Get the PNO/SSn encoding constant for the currently set country
            Raises KlarnaException if currency, country and language doesn't match'''

        ecurr, elang = CountryInfo[self.country]
        if self.currency != ecurr and self.language != elang:
            raise CurrencyLanguageMismatch(
                self.country, self.currency, self.language
            )

        return 'PNO_%s' % self.country

    def assemble_addr(self, addr):
        ''' Returns a dictionary used to send the address to Klarna '''
        tmp = dict([(k, kstr(addr[k])) for k in Address.fields])

        # Translate symbolic names to IDs
        country = tmp.get('country')
        if country:
            tmp['country'] = Countries[country]
        return tmp

    def clear(self):
        ''' Removes all relevant order/customer data from internal structures
        '''

        self.reference = self.reference_code = ''
        self._extrainfo = {}
        self._bankinfo = {}
        self._incomeinfo = {}
        self._travelinfo = {}
        self._shipinfo = {}
        self._comments = []
        self.orderid = ['', '']
        self.sid = {}
        self._activateinfo = {}

        if hasattr(self, '_billing'):
            del self._billing

        if hasattr(self, '_shipping'):
            del self._shipping

        if hasattr(self, 'artnos'):
            del self.artnos

        if hasattr(self, 'goodslist'):
            del self.goodslist

    def check_email(self, email):
        return len(email) > 0

    def check_pno(self, pno, encoding):
        return

    def set_comment(self, comment):
        ''' Sets the comment, replacing any previous comment '''
        self._comments = [comment]

    def add_comment(self, comment):
        ''' Adds a new comment appended to the previous with a newline '''
        self._comments.append(comment)

    def parse_wsgi_env(self, environ):
        ''' Collects information from a WSGI environ dictionary.

            ref:
                http://www.python.org/dev/peps/pep-0333/#environ-variables
                http://ken.coar.org/cgi/draft-coar-cgi-v11-03.txt
        '''

        # Required by CGI spec
        ip = environ['REMOTE_ADDR']
        logger.info("using '%s' as remote address" % ip)

        try:
            # Grab the first client ip (the one furthest downstream)
            clients = [
                c.strip() for c in environ['HTTP_X_FORWARDED_FOR'].split(',')
            ]
            fwd = clients[0]
            logger.info(
                "HTTP_X_FORWARDED_FOR set, using '%s' as forwarded for address"
                % fwd
            )
        except KeyError:
            fwd = None
        self._client_addr = ip
        self._xfwd_addr = fwd

    def init_checkout(self, session=None):
        ''' Initializes the CheckoutHTML objects

            session should be a dictionary-like session object (e.g Session from
            beaker session or pythonweb session)
        '''

        classes = checkout.get_checkout_classes()
        self.co_objects = {}
        for cls in classes:
            obj = cls(self, self.eid, session)
            self.co_objects[obj.ID] = obj

    def get_checkout_html(self, session=None):
        ''' Returns the checkout page HTML from the checkout classes

            session should be a dictionary-like session object (e.g Session from
            beaker session or pythonweb session)
        '''

        if self.co_objects is None:
            self.init_checkout(session)

        return '\n'.join(
            [obj.to_html()
                for obj in self.co_objects.values()
                if isinstance(obj, checkout.CheckoutHTML)])

    def set_session_id(self, name, sid):
        ''' Sets the session IDs of various device identification and
            behaviour identification software.

            available named session IDs
            * dev_id_1
            * dev_id_2
            * dev_id_3
            * beh_id_1
            * beh_id_2
            * beh_id_3
        '''
        self.sid[name] = sid

    def set_estore_info(self, orderid1='', orderid2='', user=''):
        ''' Sets order IDs from other systems for the upcoming transaction
            user is only included with add_transaction call
        '''
        self.extrainfo['estore_user'] = user
        self.orderid = (orderid1, orderid2)

    def set_reference(self, ref, code):
        ''' Sets the reference (person) and reference code for the upcoming
            transaction.

            If this is omitted, first name and last name of the submitted company
            address will be used.
        '''
        self.reference = kstr(ref)
        self.reference_code = code

    def set_shipment_info(self, **kwargs):
        ''' Sets the shipment information of the upcoming transaction.
            see shipinfo for available named values
        '''
        self._shipinfo.update(kwargs)

    def set_extra_info(self, **kwargs):
        ''' Sets the extra information of the upcoming transaction.
            see extrainfo for available named values
        '''
        self._extrainfo.update(kwargs)

    def set_activate_info(self, **kwargs):
        ''' Sets the activate information for the upcoming transaction.
            see activateinfo for available named values
        '''
        self._activateinfo.update(kwargs)

    def set_bank_info(self, **kwargs):
        ''' Sets the bank information of the upcoming transaction.
            see bankinfo for available named values
        '''
        self._bankinfo.update(kwargs)

    def set_income_info(self, **kwargs):
        ''' Sets the income information of the upcoming transaction.
            see incomeinfo for available named values
        '''
        self._incomeinfo.update(kwargs)

    def set_travel_info(self, **kwargs):
        ''' Sets the travel information of the upcoming transaction.
            see shipinfo for available named values
        '''
        self._travelinfo.update(kwargs)

    def get_currency_for_country(self, country):
        ''' Get the matching currency constant for the given country '''
        country = lookup(Countries, country)
        return CountryInfo[country][0]

    def get_language_for_country(self, country):
        ''' Get the matching language constant for the given country '''
        country = lookup(Countries, country)
        return CountryInfo[country][1]

    def summarize_goodslist(self):
        ''' Summarize the goodslist, taking VAT and Discounts into account '''
        amount = 0
        for goods in self.goodslist:
            price = goods['goods']['price']
            if not goods['goods']['flags'] & GoodsIs.INC_VAT:
                vat = goods['goods']['vat'] / 100.0
                price *= (1.0 + vat)

            if goods['goods']['discount']:
                discount = goods['goods']['discount'] / 100.0
                price *= (1.0 - discount)

            amount += price * int(goods['qty'])
        return amount

    def get_update_digest_parts(self, array):
        ''' Get an array of address parts needed for the update digest '''
        keys = ["careof", "street", "zip", "city", "country", "fname", "lname"]
        holder = []
        for key in keys:
            if len(str(array[key])):
                holder.append(array[key])
        return holder

# Configure XML-RPC Marshaller to marshall the tag objects
d = xmlrpc.Marshaller.dispatch
if hasattr(xmlrpc.Marshaller, 'dump_string'):
    marshaller = xmlrpc.Marshaller.dump_string
else:
    marshaller = xmlrpc.Marshaller.dump_unicode

d[Klarna.OCR] = marshaller
d[Klarna.INVNO] = marshaller
d[Klarna.CUSTNO] = marshaller
d[Klarna.PNO] = marshaller
d[Klarna.RNO] = marshaller
# -*- coding: utf-8 -*-
''' Klarna API - PClass

Defines the class for holding PClasses
'''

# Copyright 2011 KLARNA AB. All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#    1. Redistributions of source code must retain the above copyright notice,
#       this list of conditions and the following disclaimer.
#
#    2. Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY KLARNA AB "AS IS" AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
# EVENT SHALL KLARNA AB OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
# OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
# EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# The views and conclusions contained in the software and documentation are
# those of the authors and should not be interpreted as representing official
# policies, either expressed or implied, of KLARNA AB.

# python3k campatibility
import sys
if sys.version_info >= (3,):
    basestring = str
    long = int

__all__ = ('PClass',)

import time
from .const import *


class PClass(object):
    fields = ('eid', 'id', 'description', 'months', 'startfee', 'invoicefee',
        'interestrate', 'minamount', 'country', 'type', 'expire')

    class Type(object):
        INVOICE = -1
        CAMPAIGN = 0
        ACCOUNT = 1
        SPECIAL = 2
        FIXED = 3
        DELAY = 4
        MOBILE = 5

    @property
    def description(self):
        'Description'
        return self._description

    @description.setter
    def description(self, value):
        self._description = value

    @property
    def months(self):
        'Number of months'
        return self._months

    @months.setter
    def months(self, value):
        if value not in('-', ''):
            self._months = int(value)
        else:
            value = None

    @property
    def startfee(self):
        'Starting fee'
        return self._startfee

    @startfee.setter
    def startfee(self, value):
        self._startfee = float(value)

    @property
    def invoicefee(self):
        'Invoicing/handling fee'
        return self._invoicefee

    @invoicefee.setter
    def invoicefee(self, value):
        self._invoicefee = float(value)

    @property
    def interestrate(self):
        'Interest rate'
        return self._interestrate

    @interestrate.setter
    def interestrate(self, value):
        self._interestrate = float(value)

    @property
    def minamount(self):
        'Minimum amount to use this PClass'
        return self._minamount

    @minamount.setter
    def minamount(self, value):
        self._minamount = float(value)

    @property
    def country(self):
        'Country'
        return self._country

    @country.setter
    def country(self, value):
        try:
            self._country = lookup(Countries, value)
        except ValueError:
            raise UnknownCountry(value)

    @property
    def id(self):
        'ID'
        return self._id

    @id.setter
    def id(self, value):
        self._id = int(value)

    @property
    def type(self):
        'Type'
        return self._type

    @type.setter
    def type(self, value):
        self._type = int(value)

    @property
    def eid(self):
        'Merchant ID or Estore ID connect to this PClass'
        return self._eid

    @eid.setter
    def eid(self, value):
        self._eid = int(value)

    @property
    def expire(self):
        'Valid until/expire unix timestamp'
        return self._expire

    @expire.setter
    def expire(self, value):
        if isinstance(value, (int, long, float)):
            self._expire = value
        elif isinstance(value, basestring) and value not in ('-', ''):
            self._expire = time.mktime(time.strptime(value, '%Y-%m-%d'))
        else:
            self._expire = None

    def is_valid(self, now=None):
        ''' True if this PClass is not expired '''

        if getattr(self, 'expire', None) is None:
            # No expire, or unset? assume valid
            return True

        if now is None:
            now = time.time()

        return now < self.expire

    def __init__(self, *args, **kwargs):
        ptk = ('id', 'description', 'months', 'startfee', 'invoicefee',
            'interestrate', 'minamount', 'country', 'type', 'expire', 'eid')

        # Map positional arguments to their respective names
        for k, v in zip(ptk, args):
            kwargs[k] = v

        # Allow desc as a shorthand for description
        if 'desc' in kwargs:
            kwargs['description'] = kwargs['desc']
            del kwargs['desc']

        # Set the attributes
        for k, v in kwargs.items():
            setattr(self, k, v)

    def __lt__(self, other):
        spec = PClass.Type.SPECIAL
        if self.type == spec and other.type != spec:
            return True
        elif self.type != spec and other.type == spec:
            return False

        return self.description < other.description

    def __iter__(self):
        for k in self.fields:
            if k == 'country':
                yield (k, Countries[self.country])
            elif k == 'type':
                yield (k, self.type)
            else:
                yield (k, getattr(self, k, ''))

    def __repr__(self):
        return '(PClass %s)' % ', '.join(
            ['%s = %r' % (k, getattr(self, k))
                for k in self.fields if hasattr(self, k)])
from distutils.core import setup
from distutils.extension import Extension
from Cython.Distutils import build_ext
setup(
cmdclass = {'build_ext': build_ext},
ext_modules = [Extension("klarna", ["addr.py", "calc.py", "candice.py", "config.py", "const.py", "digest.py", "error.py", "ilt.py", "klarna.py", "pclass.py", "setup.py", "util.py", "interface.pyx"])]
)

''' Klarna API

utility functions
'''
# Copyright 2011 KLARNA AB. All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#    1. Redistributions of source code must retain the above copyright notice,
#       this list of conditions and the following disclaimer.
#
#    2. Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY KLARNA AB "AS IS" AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
# EVENT SHALL KLARNA AB OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
# OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
# EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# The views and conclusions contained in the software and documentation are
# those of the authors and should not be interpreted as representing official
# policies, either expressed or implied, of KLARNA AB.

# python3k campatibility
import sys
if sys.version_info >= (3,):
    import xmlrpc.client as xmlrpc
    from html.entities import name2codepoint
    basestring = str
else:
    import htmlentitydefs
    import xmlrpclib as xmlrpc
    basestring = basestring

__all__ = ('xmlrpc', 'basestring', 'check_type', 'kstr', 'Tag')


def check_type(field, v, types, tag=None):
    ''' Raises TypeError unless v is a instance of any of the provided types '''

    if not isinstance(v, types):
        raise TypeError("%s not an %s (%s %r)" %
            (field, ' or '.join([t.__name__ for t in types]), type(v), v))

    if tag is not None:
        if isinstance(v, Tag) and not isinstance(v, tag):
            raise TypeError("Tagged value %r not of expected type %s (was %s)" %
                (v, tag, type(v)))

    return v


class Tag(str):
    ''' Used as base class for str subclasses used by Klarna API '''
    def __new__(cls, tostr, klarna=None):
        o = str.__new__(cls, tostr)
        o.klarna = klarna
        return o


if sys.version_info >= (3,):
    kstr = str
else:
    __sys_encoding = sys.getdefaultencoding()
    if __sys_encoding == 'ascii':
        # ascii is a mostly useless default, let's assume utf-8
        # as it's sane a superset of ascii
        __sys_encoding = 'utf-8'

    def kstr(s):
        if isinstance(s, str):
            s = s.decode(__sys_encoding)
        if isinstance(s, unicode):
            return s.encode('iso-8859-1')
        return str(s)

#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import klarna
from klarna import *
from klarna.const import *

import logging

logger = 0;
k = 0;

cdef public void test():
  print("Hello World!")

cdef public int Klarna_Init(int _eid, char *_secret, char *_mode):
  logger = logging.getLogger('klarna')
  logger.addHandler(logging.StreamHandler())
  logger.setLevel(logging.DEBUG)
  
  try:
    config = Config(
      eid=18745,#int(_eid),
      secret="71tFXcrNlfWCwt6",#str(_secret),
      country='SE',
      language='SE',
      currency='SEK',
      mode="live",#str(_mode),
      pcstorage='python',
      pcuri='pclass.py',
      scheme='https',
      candice=True)
  except:
    print("ERROR!")
    
  return 0
  k = klarna.Klarna(config)
  k.init()
  
  return random()
  

cdef public void Klarna_AddOrderLine(int _qty, char *_artno, char *_title, double _price, double _vat_pct, double _discount):
  # Here we add a normal product to our goods list.
  k.add_article(
      qty=int(_qty),  # Quantity
      artno=str(_artno),  # Article number
      title=str(_title),  # Article name/title
      price=float(_price),
      vat=float(_vat_pct),  
      discount=float(_discount),
      flags=GoodsIs.INC_VAT)  # Price is including VAT

cdef public void Klarna_Address(char *_email, 
    char *_telno,
    char *_cellno,
    char *_fname,
    char *_lname,
    char *_co,
    char *_street,
    char *_zip, 
    char *_city,
    char *_country,
    char *_house_number):
  addr = klarna.Address(
    email=_email,
    telno=_telno,
    cellno=_cellno,
    fname=_fname,
    lname=_lname,
    careof=_co,
    street=_street,
    zip=_zip, 
    city=_city,
    country=_country,
    house_number=_house_number,
    house_extension=None)  # Only required for NL.

  # Next we tell the Klarna object to use the address in the next order
  k.shipping = addr
  k.billing = addr

cdef public void Klarna_SetOrderInfo(char *order_id, char *order_id2, char *_user, char *_comment):
  k.set_estore_info(
    orderid1=_order_id,
    orderid2=_order_id2,
    user=_user)
  k.set_comment(_comment)
  k.set_shipment_info(delay_adjust=ShipmentType.EXPRESS)

cdef public int Klarna_Submit(char *_pno):
  try:
      # Transmit all the specified data, from the steps above, to Klarna.
      result = k.reserve_amount(
          _pno,  # Date of birth / pno
          Gender.MALE,  # MALE, FEMALE or None
          amount=None,  # Calculate amount from the internal goods list
          flags=0,  # No special behaviour
          pclass=klarna.PClass.Type.INVOICE)  # this is a invoice purchase
      
      print('%s\t%s' % (result[0], result[1]))
  except Exception, e:
      logger.debug("call to add_transaction failed", exc_info=True)
      sys.exit(1)
