package com.example.dbflute.scala.dbflute.cbean.cq.bs;

import java.lang.Long;
import java.util._;

import org.seasar.dbflute.cbean._;
import org.seasar.dbflute.cbean.AbstractConditionQuery._;
import org.seasar.dbflute.cbean.chelper._;
import org.seasar.dbflute.cbean.ckey._;
import org.seasar.dbflute.cbean.coption._;
import org.seasar.dbflute.cbean.cvalue.ConditionValue;
import org.seasar.dbflute.cbean.sqlclause.SqlClause;
import org.seasar.dbflute.dbmeta.DBMetaProvider;
import com.example.dbflute.scala.dbflute.allcommon._;
import com.example.dbflute.scala.dbflute.cbean._;
import com.example.dbflute.scala.dbflute.cbean.cq._;

/**
 * The abstract condition-query of SERVICE_RANK.
 * @author DBFlute(AutoGenerator)
 */
abstract class AbstractBsServiceRankCQ(referrerQuery: ConditionQuery, sqlClause: SqlClause, aliasName: String, nestLevel: Integer)
        extends AbstractConditionQuery(referrerQuery, sqlClause, aliasName, nestLevel) {

    // ===================================================================================
    //                                                                     DBMeta Provider
    //                                                                     ===============
    @Override
    protected def xgetDBMetaProvider(): DBMetaProvider = {
        return DBMetaInstanceHandler.getProvider();
    }

    // ===================================================================================
    //                                                                          Table Name
    //                                                                          ==========
    def getTableDbName(): String = {
        return "SERVICE_RANK";
    }

    // ===================================================================================
    //                                                                               Query
    //                                                                               =====

    /**
     * Equal(=). And NullOrEmptyIgnored, OnlyOnceRegistered. <br />
     * (サービスランクコード)SERVICE_RANK_CODE: {PK, NotNull, CHAR(3), classification=ServiceRank}
     * @param serviceRankCode The value of serviceRankCode as equal. (NullAllowed: if null (or empty), no condition)
     */
     def setServiceRankCode_Equal(serviceRankCode: String): Unit = {
        doSetServiceRankCode_Equal(fRES(serviceRankCode));
    }

    /**
     * Equal(=). As ServiceRank. And NullOrEmptyIgnored, OnlyOnceRegistered. <br />
     * (サービスランクコード)SERVICE_RANK_CODE: {PK, NotNull, CHAR(3), classification=ServiceRank} <br />
     * rank of service member gets
     * @param cdef The instance of classification definition (as ENUM type). (NullAllowed: if null, no condition)
     */
    def setServiceRankCode_Equal_AsServiceRank(cdef: CDef.ServiceRank): Unit = {
        doSetServiceRankCode_Equal(if (cdef != null) { cdef.code } else { null });
    }

    /**
     * Equal(=). As Platinum (PLT). And OnlyOnceRegistered. <br />
     * PLATINUM: platinum rank
     */
    def setServiceRankCode_Equal_Platinum(): Unit = {
        setServiceRankCode_Equal_AsServiceRank(CDef.ServiceRank.Platinum);
    }

    /**
     * Equal(=). As Gold (GLD). And OnlyOnceRegistered. <br />
     * GOLD: gold rank
     */
    def setServiceRankCode_Equal_Gold(): Unit = {
        setServiceRankCode_Equal_AsServiceRank(CDef.ServiceRank.Gold);
    }

    /**
     * Equal(=). As Silver (SIL). And OnlyOnceRegistered. <br />
     * SILVER: silver rank
     */
    def setServiceRankCode_Equal_Silver(): Unit = {
        setServiceRankCode_Equal_AsServiceRank(CDef.ServiceRank.Silver);
    }

    /**
     * Equal(=). As Bronze (BRZ). And OnlyOnceRegistered. <br />
     * BRONZE: bronze rank
     */
    def setServiceRankCode_Equal_Bronze(): Unit = {
        setServiceRankCode_Equal_AsServiceRank(CDef.ServiceRank.Bronze);
    }

    /**
     * Equal(=). As Plastic (PLS). And OnlyOnceRegistered. <br />
     * PLASTIC: plastic rank
     */
    def setServiceRankCode_Equal_Plastic(): Unit = {
        setServiceRankCode_Equal_AsServiceRank(CDef.ServiceRank.Plastic);
    }

    protected def doSetServiceRankCode_Equal(serviceRankCode: String): Unit = {
        regServiceRankCode(CK_EQ, hSC("SERVICE_RANK_CODE", serviceRankCode, 3, "E"));
    }

    /**
     * NotEqual(&lt;&gt;). And NullOrEmptyIgnored, OnlyOnceRegistered. <br />
     * (サービスランクコード)SERVICE_RANK_CODE: {PK, NotNull, CHAR(3), classification=ServiceRank}
     * @param serviceRankCode The value of serviceRankCode as notEqual. (NullAllowed: if null (or empty), no condition)
     */
     def setServiceRankCode_NotEqual(serviceRankCode: String): Unit = {
        doSetServiceRankCode_NotEqual(fRES(serviceRankCode));
    }

    /**
     * NotEqual(&lt;&gt;). As ServiceRank. And NullOrEmptyIgnored, OnlyOnceRegistered. <br />
     * (サービスランクコード)SERVICE_RANK_CODE: {PK, NotNull, CHAR(3), classification=ServiceRank} <br />
     * rank of service member gets
     * @param cdef The instance of classification definition (as ENUM type). (NullAllowed: if null, no condition)
     */
    def setServiceRankCode_NotEqual_AsServiceRank(cdef: CDef.ServiceRank): Unit = {
        doSetServiceRankCode_NotEqual(if (cdef != null) { cdef.code } else { null });
    }

    /**
     * NotEqual(&lt;&gt;). As Platinum (PLT). And OnlyOnceRegistered. <br />
     * PLATINUM: platinum rank
     */
    def setServiceRankCode_NotEqual_Platinum(): Unit = {
        setServiceRankCode_NotEqual_AsServiceRank(CDef.ServiceRank.Platinum);
    }

    /**
     * NotEqual(&lt;&gt;). As Gold (GLD). And OnlyOnceRegistered. <br />
     * GOLD: gold rank
     */
    def setServiceRankCode_NotEqual_Gold(): Unit = {
        setServiceRankCode_NotEqual_AsServiceRank(CDef.ServiceRank.Gold);
    }

    /**
     * NotEqual(&lt;&gt;). As Silver (SIL). And OnlyOnceRegistered. <br />
     * SILVER: silver rank
     */
    def setServiceRankCode_NotEqual_Silver(): Unit = {
        setServiceRankCode_NotEqual_AsServiceRank(CDef.ServiceRank.Silver);
    }

    /**
     * NotEqual(&lt;&gt;). As Bronze (BRZ). And OnlyOnceRegistered. <br />
     * BRONZE: bronze rank
     */
    def setServiceRankCode_NotEqual_Bronze(): Unit = {
        setServiceRankCode_NotEqual_AsServiceRank(CDef.ServiceRank.Bronze);
    }

    /**
     * NotEqual(&lt;&gt;). As Plastic (PLS). And OnlyOnceRegistered. <br />
     * PLASTIC: plastic rank
     */
    def setServiceRankCode_NotEqual_Plastic(): Unit = {
        setServiceRankCode_NotEqual_AsServiceRank(CDef.ServiceRank.Plastic);
    }

    protected def doSetServiceRankCode_NotEqual(serviceRankCode: String): Unit = {
        regServiceRankCode(CK_NES, hSC("SERVICE_RANK_CODE", serviceRankCode, 3, "E"));
    }

    /**
     * InScope {in ('a', 'b')}. And NullOrEmptyIgnored, NullOrEmptyElementIgnored, SeveralRegistered. <br />
     * (サービスランクコード)SERVICE_RANK_CODE: {PK, NotNull, CHAR(3), classification=ServiceRank}
     * @param serviceRankCodeList The collection of serviceRankCode as inScope. (NullAllowed: if null (or empty), no condition)
     */
    def setServiceRankCode_InScope(serviceRankCodeList: Collection[String]): Unit = {
        doSetServiceRankCode_InScope(serviceRankCodeList);
    }

    /**
     * InScope {in ('a', 'b')}. As ServiceRank. And NullOrEmptyIgnored, NullOrEmptyElementIgnored, SeveralRegistered. <br />
     * (サービスランクコード)SERVICE_RANK_CODE: {PK, NotNull, CHAR(3), classification=ServiceRank} <br />
     * rank of service member gets
     * @param cdefList The list of classification definition (as ENUM type). (NullAllowed: if null (or empty), no condition)
     */
    def setServiceRankCode_InScope_AsServiceRank(cdefList: Collection[CDef.ServiceRank]): Unit = {
        doSetServiceRankCode_InScope(cTStrL(cdefList));
    }

    def doSetServiceRankCode_InScope(serviceRankCodeList: Collection[String]): Unit = {
        regINS(CK_INS, cTL(serviceRankCodeList), getCValueServiceRankCode(), "SERVICE_RANK_CODE");
    }

    /**
     * NotInScope {not in ('a', 'b')}. And NullOrEmptyIgnored, NullOrEmptyElementIgnored, SeveralRegistered. <br />
     * (サービスランクコード)SERVICE_RANK_CODE: {PK, NotNull, CHAR(3), classification=ServiceRank}
     * @param serviceRankCodeList The collection of serviceRankCode as notInScope. (NullAllowed: if null (or empty), no condition)
     */
    def setServiceRankCode_NotInScope(serviceRankCodeList: Collection[String]): Unit = {
        doSetServiceRankCode_NotInScope(serviceRankCodeList);
    }

    /**
     * NotInScope {not in ('a', 'b')}. As ServiceRank. And NullOrEmptyIgnored, NullOrEmptyElementIgnored, SeveralRegistered. <br />
     * (サービスランクコード)SERVICE_RANK_CODE: {PK, NotNull, CHAR(3), classification=ServiceRank} <br />
     * rank of service member gets
     * @param cdefList The list of classification definition (as ENUM type). (NullAllowed: if null (or empty), no condition)
     */
    def setServiceRankCode_NotInScope_AsServiceRank(cdefList: Collection[CDef.ServiceRank]): Unit = {
        doSetServiceRankCode_NotInScope(cTStrL(cdefList));
    }

    def doSetServiceRankCode_NotInScope(serviceRankCodeList: Collection[String]): Unit = {
        regINS(CK_NINS, cTL(serviceRankCodeList), getCValueServiceRankCode(), "SERVICE_RANK_CODE");
    }

    /**
     * Set up ExistsReferrer (correlated sub-query). <br />
     * {exists (select SERVICE_RANK_CODE from MEMBER_SERVICE where ...)} <br />
     * (会員サービス)MEMBER_SERVICE by SERVICE_RANK_CODE, named 'memberServiceAsOne'.
     * <pre>
     * cb.query().<span style="color: #DD4747">existsMemberServiceList</span>(new SubQuery&lt;MemberServiceCB&gt;() {
     *     public void query(MemberServiceCB subCB) {
     *         subCB.query().setXxx...
     *     }
     * });
     * </pre>
     * @param subQuery The sub-query of MemberServiceList for 'exists'. (NotNull)
     */
    def existsMemberServiceList(subQuery: (MemberServiceCB) => Unit): Unit = {
        assertObjectNotNull("subQuery", subQuery);
        val cb: MemberServiceCB = new MemberServiceCB(); cb.xsetupForExistsReferrer(this);
        try { lock(); subQuery(cb); } finally { unlock(); }
        val pp: String = keepServiceRankCode_ExistsReferrer_MemberServiceList(cb.query());
        registerExistsReferrer(cb.query(), "SERVICE_RANK_CODE", "SERVICE_RANK_CODE", pp, "memberServiceList");
    }
    def keepServiceRankCode_ExistsReferrer_MemberServiceList(sq: MemberServiceCQ): String;

    /**
     * Set up NotExistsReferrer (correlated sub-query). <br />
     * {not exists (select SERVICE_RANK_CODE from MEMBER_SERVICE where ...)} <br />
     * (会員サービス)MEMBER_SERVICE by SERVICE_RANK_CODE, named 'memberServiceAsOne'.
     * <pre>
     * cb.query().<span style="color: #DD4747">notExistsMemberServiceList</span>(new SubQuery&lt;MemberServiceCB&gt;() {
     *     public void query(MemberServiceCB subCB) {
     *         subCB.query().setXxx...
     *     }
     * });
     * </pre>
     * @param subQuery The sub-query of ServiceRankCode_NotExistsReferrer_MemberServiceList for 'not exists'. (NotNull)
     */
    def notExistsMemberServiceList(subQuery: (MemberServiceCB) => Unit): Unit = {
        assertObjectNotNull("subQuery", subQuery);
        val cb: MemberServiceCB = new MemberServiceCB(); cb.xsetupForExistsReferrer(this);
        try { lock(); subQuery(cb); } finally { unlock(); }
        val pp: String = keepServiceRankCode_NotExistsReferrer_MemberServiceList(cb.query());
        registerNotExistsReferrer(cb.query(), "SERVICE_RANK_CODE", "SERVICE_RANK_CODE", pp, "memberServiceList");
    }
    def keepServiceRankCode_NotExistsReferrer_MemberServiceList(sq: MemberServiceCQ): String;

    /**
     * Set up InScopeRelation (sub-query). <br />
     * {in (select SERVICE_RANK_CODE from MEMBER_SERVICE where ...)} <br />
     * (会員サービス)MEMBER_SERVICE by SERVICE_RANK_CODE, named 'memberServiceAsOne'.
     * @param subQuery The sub-query of MemberServiceList for 'in-scope'. (NotNull)
     */
    def inScopeMemberServiceList(subQuery: (MemberServiceCB) => Unit): Unit = {
        assertObjectNotNull("subQuery", subQuery);
        val cb: MemberServiceCB = new MemberServiceCB(); cb.xsetupForInScopeRelation(this);
        try { lock(); subQuery(cb); } finally { unlock(); }
        val pp: String = keepServiceRankCode_InScopeRelation_MemberServiceList(cb.query());
        registerInScopeRelation(cb.query(), "SERVICE_RANK_CODE", "SERVICE_RANK_CODE", pp, "memberServiceList");
    }
    def keepServiceRankCode_InScopeRelation_MemberServiceList(sq: MemberServiceCQ): String;

    /**
     * Set up NotInScopeRelation (sub-query). <br />
     * {not in (select SERVICE_RANK_CODE from MEMBER_SERVICE where ...)} <br />
     * (会員サービス)MEMBER_SERVICE by SERVICE_RANK_CODE, named 'memberServiceAsOne'.
     * @param subQuery The sub-query of MemberServiceList for 'not in-scope'. (NotNull)
     */
    def notInScopeMemberServiceList(subQuery: (MemberServiceCB) => Unit): Unit = {
        assertObjectNotNull("subQuery", subQuery);
        val cb: MemberServiceCB = new MemberServiceCB(); cb.xsetupForInScopeRelation(this);
        try { lock(); subQuery(cb); } finally { unlock(); }
        val pp: String = keepServiceRankCode_NotInScopeRelation_MemberServiceList(cb.query());
        registerNotInScopeRelation(cb.query(), "SERVICE_RANK_CODE", "SERVICE_RANK_CODE", pp, "memberServiceList");
    }
    def keepServiceRankCode_NotInScopeRelation_MemberServiceList(sq: MemberServiceCQ): String;

    def xsderiveMemberServiceList(fn: String, sq: SubQuery[MemberServiceCB], al: String, op: DerivedReferrerOption): Unit = {
        assertObjectNotNull("subQuery", sq);
        val cb: MemberServiceCB = new MemberServiceCB(); cb.xsetupForDerivedReferrer(this);
        try { lock(); sq.query(cb); } finally { unlock(); }
        val pp: String = keepServiceRankCode_SpecifyDerivedReferrer_MemberServiceList(cb.query());
        registerSpecifyDerivedReferrer(fn, cb.query(), "SERVICE_RANK_CODE", "SERVICE_RANK_CODE", pp, "memberServiceList", al, op);
    }
    def keepServiceRankCode_SpecifyDerivedReferrer_MemberServiceList(sq: MemberServiceCQ): String;

    /**
     * Prepare for (Query)DerivedReferrer (correlated sub-query). <br />
     * {FOO &lt;= (select max(BAR) from MEMBER_SERVICE where ...)} <br />
     * (会員サービス)MEMBER_SERVICE by SERVICE_RANK_CODE, named 'memberServiceAsOne'.
     * <pre>
     * cb.query().<span style="color: #DD4747">derivedMemberServiceList()</span>.<span style="color: #DD4747">max</span>(new SubQuery&lt;MemberServiceCB&gt;() {
     *     public void query(MemberServiceCB subCB) {
     *         subCB.specify().<span style="color: #DD4747">columnFoo...</span> <span style="color: #3F7E5E">// derived column by function</span>
     *         subCB.query().setBar... <span style="color: #3F7E5E">// referrer condition</span>
     *     }
     * }).<span style="color: #DD4747">greaterEqual</span>(123); <span style="color: #3F7E5E">// condition to derived column</span>
     * </pre>
     * @return The object to set up a function for referrer table. (NotNull)
     */
    def derivedMemberServiceList(): HpQDRFunction[MemberServiceCB] = {
        return xcreateQDRFunctionMemberServiceList();
    }
    protected def xcreateQDRFunctionMemberServiceList(): HpQDRFunction[MemberServiceCB] = {
        return new HpQDRFunction[MemberServiceCB](new HpQDRSetupper[MemberServiceCB]() {
            def setup(fn: String, sq: SubQuery[MemberServiceCB], rd: String, vl: Object, op: DerivedReferrerOption): Unit = {
                xqderiveMemberServiceList(fn, sq, rd, vl, op);
            }
        });
    }
    def xqderiveMemberServiceList(fn: String, sq: SubQuery[MemberServiceCB], rd: String, vl: Object, op: DerivedReferrerOption): Unit = {
        assertObjectNotNull("subQuery", sq);
        val cb: MemberServiceCB = new MemberServiceCB(); cb.xsetupForDerivedReferrer(this);
        try { lock(); sq.query(cb); } finally { unlock(); }
        val sqpp: String = keepServiceRankCode_QueryDerivedReferrer_MemberServiceList(cb.query());
        val prpp: String = keepServiceRankCode_QueryDerivedReferrer_MemberServiceListParameter(vl);
        registerQueryDerivedReferrer(fn, cb.query(), "SERVICE_RANK_CODE", "SERVICE_RANK_CODE", sqpp, "memberServiceList", rd, vl, prpp, op);
    }
    def keepServiceRankCode_QueryDerivedReferrer_MemberServiceList(sq: MemberServiceCQ): String;
    def keepServiceRankCode_QueryDerivedReferrer_MemberServiceListParameter(vl: Object): String;

    /**
     * IsNull {is null}. And OnlyOnceRegistered. <br />
     * (サービスランクコード)SERVICE_RANK_CODE: {PK, NotNull, CHAR(3), classification=ServiceRank}
     */
    def setServiceRankCode_IsNull(): Unit = { regServiceRankCode(CK_ISN, AbstractConditionQuery.DOBJ); }

    /**
     * IsNotNull {is not null}. And OnlyOnceRegistered. <br />
     * (サービスランクコード)SERVICE_RANK_CODE: {PK, NotNull, CHAR(3), classification=ServiceRank}
     */
    def setServiceRankCode_IsNotNull(): Unit = { regServiceRankCode(CK_ISNN, AbstractConditionQuery.DOBJ); }

    protected def regServiceRankCode(ky: ConditionKey, vl: Object): Unit = { regQ(ky, vl, getCValueServiceRankCode(), "SERVICE_RANK_CODE"); }
    protected def getCValueServiceRankCode(): ConditionValue;

    /**
     * Equal(=). And NullOrEmptyIgnored, OnlyOnceRegistered. <br />
     * (サービスランク名称)SERVICE_RANK_NAME: {NotNull, VARCHAR(50)}
     * @param serviceRankName The value of serviceRankName as equal. (NullAllowed: if null (or empty), no condition)
     */
     def setServiceRankName_Equal(serviceRankName: String): Unit = {
        doSetServiceRankName_Equal(fRES(serviceRankName));
    }

    protected def doSetServiceRankName_Equal(serviceRankName: String): Unit = {
        regServiceRankName(CK_EQ, serviceRankName);
    }

    /**
     * PrefixSearch {like 'xxx%' escape ...}. And NullOrEmptyIgnored, SeveralRegistered. <br />
     * (サービスランク名称)SERVICE_RANK_NAME: {NotNull, VARCHAR(50)}
     * @param serviceRankName The value of serviceRankName as prefixSearch. (NullAllowed: if null (or empty), no condition)
     */
    def setServiceRankName_PrefixSearch(serviceRankName: String): Unit = {
        setServiceRankName_LikeSearch(serviceRankName, cLSOP());
    }

    /**
     * LikeSearch with various options. (versatile) {like '%xxx%' escape ...}. And NullOrEmptyIgnored, SeveralRegistered. <br />
     * (サービスランク名称)SERVICE_RANK_NAME: {NotNull, VARCHAR(50)} <br />
     * <pre>e.g. setServiceRankName_LikeSearch("xxx", new <span style="color: #DD4747">LikeSearchOption</span>().likeContain());</pre>
     * @param serviceRankName The value of serviceRankName as likeSearch. (NullAllowed: if null (or empty), no condition)
     * @param likeSearchOption The option of like-search. (NotNull)
     */
    def setServiceRankName_LikeSearch(serviceRankName: String, likeSearchOption: LikeSearchOption): Unit = {
        regLSQ(CK_LS, fRES(serviceRankName), getCValueServiceRankName(), "SERVICE_RANK_NAME", likeSearchOption);
    }

    /**
     * NotLikeSearch with various options. (versatile) {not like 'xxx%' escape ...} <br />
     * And NullOrEmptyIgnored, SeveralRegistered. <br />
     * (サービスランク名称)SERVICE_RANK_NAME: {NotNull, VARCHAR(50)}
     * @param serviceRankName The value of serviceRankName as notLikeSearch. (NullAllowed: if null (or empty), no condition)
     * @param likeSearchOption The option of not-like-search. (NotNull)
     */
    def setServiceRankName_NotLikeSearch(serviceRankName: String, likeSearchOption: LikeSearchOption): Unit = {
        regLSQ(CK_NLS, fRES(serviceRankName), getCValueServiceRankName(), "SERVICE_RANK_NAME", likeSearchOption);
    }

    protected def regServiceRankName(ky: ConditionKey, vl: Object): Unit = { regQ(ky, vl, getCValueServiceRankName(), "SERVICE_RANK_NAME"); }
    protected def getCValueServiceRankName(): ConditionValue;
    
    /**
     * Equal(=). And NullIgnored, OnlyOnceRegistered. <br />
     * (サービスポイント発生率)SERVICE_POINT_INCIDENCE: {NotNull, DECIMAL(5, 3)}
     * @param servicePointIncidence The value of servicePointIncidence as equal. (NullAllowed: if null, no condition)
     */
     def setServicePointIncidence_Equal(servicePointIncidence: java.math.BigDecimal): Unit = {
        doSetServicePointIncidence_Equal(servicePointIncidence);
    }

    protected def doSetServicePointIncidence_Equal(servicePointIncidence: java.math.BigDecimal): Unit = {
        regServicePointIncidence(CK_EQ, servicePointIncidence);
    }

    /**
     * NotEqual(&lt;&gt;). And NullIgnored, OnlyOnceRegistered. <br />
     * (サービスポイント発生率)SERVICE_POINT_INCIDENCE: {NotNull, DECIMAL(5, 3)}
     * @param servicePointIncidence The value of servicePointIncidence as notEqual. (NullAllowed: if null, no condition)
     */
     def setServicePointIncidence_NotEqual(servicePointIncidence: java.math.BigDecimal): Unit = {
        doSetServicePointIncidence_NotEqual(servicePointIncidence);
    }

    protected def doSetServicePointIncidence_NotEqual(servicePointIncidence: java.math.BigDecimal): Unit = {
        regServicePointIncidence(CK_NES, servicePointIncidence);
    }

    /**
     * GreaterThan(&gt;). And NullIgnored, OnlyOnceRegistered. <br />
     * (サービスポイント発生率)SERVICE_POINT_INCIDENCE: {NotNull, DECIMAL(5, 3)}
     * @param servicePointIncidence The value of servicePointIncidence as greaterThan. (NullAllowed: if null, no condition)
     */
    def setServicePointIncidence_GreaterThan(servicePointIncidence: java.math.BigDecimal): Unit = {
        regServicePointIncidence(CK_GT, servicePointIncidence);
    }

    /**
     * LessThan(&lt;). And NullIgnored, OnlyOnceRegistered. <br />
     * (サービスポイント発生率)SERVICE_POINT_INCIDENCE: {NotNull, DECIMAL(5, 3)}
     * @param servicePointIncidence The value of servicePointIncidence as lessThan. (NullAllowed: if null, no condition)
     */
    def setServicePointIncidence_LessThan(servicePointIncidence: java.math.BigDecimal): Unit = {
        regServicePointIncidence(CK_LT, servicePointIncidence);
    }

    /**
     * GreaterEqual(&gt;=). And NullIgnored, OnlyOnceRegistered. <br />
     * (サービスポイント発生率)SERVICE_POINT_INCIDENCE: {NotNull, DECIMAL(5, 3)}
     * @param servicePointIncidence The value of servicePointIncidence as greaterEqual. (NullAllowed: if null, no condition)
     */
    def setServicePointIncidence_GreaterEqual(servicePointIncidence: java.math.BigDecimal): Unit = {
        regServicePointIncidence(CK_GE, servicePointIncidence);
    }

    /**
     * LessEqual(&lt;=). And NullIgnored, OnlyOnceRegistered. <br />
     * (サービスポイント発生率)SERVICE_POINT_INCIDENCE: {NotNull, DECIMAL(5, 3)}
     * @param servicePointIncidence The value of servicePointIncidence as lessEqual. (NullAllowed: if null, no condition)
     */
    def setServicePointIncidence_LessEqual(servicePointIncidence: java.math.BigDecimal): Unit = {
        regServicePointIncidence(CK_LE, servicePointIncidence);
    }

    /**
     * RangeOf with various options. (versatile) <br />
     * {(default) minNumber &lt;= column &lt;= maxNumber} <br />
     * And NullIgnored, OnlyOnceRegistered. <br />
     * (サービスポイント発生率)SERVICE_POINT_INCIDENCE: {NotNull, DECIMAL(5, 3)}
     * @param minNumber The min number of servicePointIncidence. (NullAllowed: if null, no from-condition)
     * @param maxNumber The max number of servicePointIncidence. (NullAllowed: if null, no to-condition)
     * @param rangeOfOption The option of range-of. (NotNull)
     */
    def setServicePointIncidence_RangeOf(minNumber: java.math.BigDecimal, maxNumber: java.math.BigDecimal, rangeOfOption: RangeOfOption): Unit = {
        regROO(minNumber, maxNumber, getCValueServicePointIncidence(), "SERVICE_POINT_INCIDENCE", rangeOfOption);
    }

    /**
     * InScope {in (1, 2)}. And NullIgnored, NullElementIgnored, SeveralRegistered. <br />
     * (サービスポイント発生率)SERVICE_POINT_INCIDENCE: {NotNull, DECIMAL(5, 3)}
     * @param servicePointIncidenceList The collection of servicePointIncidence as inScope. (NullAllowed: if null (or empty), no condition)
     */
    def setServicePointIncidence_InScope(servicePointIncidenceList: Collection[java.math.BigDecimal]): Unit = {
        doSetServicePointIncidence_InScope(servicePointIncidenceList);
    }

    protected def doSetServicePointIncidence_InScope(servicePointIncidenceList: Collection[java.math.BigDecimal]): Unit = {
        regINS(CK_INS, cTL(servicePointIncidenceList), getCValueServicePointIncidence(), "SERVICE_POINT_INCIDENCE");
    }

    /**
     * NotInScope {not in (1, 2)}. And NullIgnored, NullElementIgnored, SeveralRegistered. <br />
     * (サービスポイント発生率)SERVICE_POINT_INCIDENCE: {NotNull, DECIMAL(5, 3)}
     * @param servicePointIncidenceList The collection of servicePointIncidence as notInScope. (NullAllowed: if null (or empty), no condition)
     */
    def setServicePointIncidence_NotInScope(servicePointIncidenceList: Collection[java.math.BigDecimal]): Unit = {
        doSetServicePointIncidence_NotInScope(servicePointIncidenceList);
    }

    protected def doSetServicePointIncidence_NotInScope(servicePointIncidenceList: Collection[java.math.BigDecimal]): Unit = {
        regINS(CK_NINS, cTL(servicePointIncidenceList), getCValueServicePointIncidence(), "SERVICE_POINT_INCIDENCE");
    }

    protected def regServicePointIncidence(ky: ConditionKey, vl: Object): Unit = { regQ(ky, vl, getCValueServicePointIncidence(), "SERVICE_POINT_INCIDENCE"); }
    protected def getCValueServicePointIncidence(): ConditionValue;
    
    /**
     * Equal(=). And NullIgnored, OnlyOnceRegistered. <br />
     * (新規受け入れ可能フラグ)NEW_ACCEPTABLE_FLG: {NotNull, INTEGER(10), classification=Flg}
     * @param newAcceptableFlg The value of newAcceptableFlg as equal. (NullAllowed: if null, no condition)
     */
     def setNewAcceptableFlg_Equal(newAcceptableFlg: Integer): Unit = {
        doSetNewAcceptableFlg_Equal(newAcceptableFlg);
    }

    /**
     * Equal(=). As Flg. And NullIgnored, OnlyOnceRegistered. <br />
     * (新規受け入れ可能フラグ)NEW_ACCEPTABLE_FLG: {NotNull, INTEGER(10), classification=Flg} <br />
     * general boolean classification for every flg-column
     * @param cdef The instance of classification definition (as ENUM type). (NullAllowed: if null, no condition)
     */
    def setNewAcceptableFlg_Equal_AsFlg(cdef: CDef.Flg): Unit = {
        doSetNewAcceptableFlg_Equal(cTNum(if (cdef != null) { cdef.code } else { null }, classOf[Integer]));
    }

    /**
     * Equal(=). As True (1). And NullIgnored, OnlyOnceRegistered. <br />
     * Yes: means valid
     */
    def setNewAcceptableFlg_Equal_True(): Unit = {
        setNewAcceptableFlg_Equal_AsFlg(CDef.Flg.True);
    }

    /**
     * Equal(=). As False (0). And NullIgnored, OnlyOnceRegistered. <br />
     * No: means invalid
     */
    def setNewAcceptableFlg_Equal_False(): Unit = {
        setNewAcceptableFlg_Equal_AsFlg(CDef.Flg.False);
    }

    protected def doSetNewAcceptableFlg_Equal(newAcceptableFlg: Integer): Unit = {
        regNewAcceptableFlg(CK_EQ, newAcceptableFlg);
    }

    /**
     * NotEqual(&lt;&gt;). And NullIgnored, OnlyOnceRegistered. <br />
     * (新規受け入れ可能フラグ)NEW_ACCEPTABLE_FLG: {NotNull, INTEGER(10), classification=Flg}
     * @param newAcceptableFlg The value of newAcceptableFlg as notEqual. (NullAllowed: if null, no condition)
     */
     def setNewAcceptableFlg_NotEqual(newAcceptableFlg: Integer): Unit = {
        doSetNewAcceptableFlg_NotEqual(newAcceptableFlg);
    }

    /**
     * NotEqual(&lt;&gt;). As Flg. And NullIgnored, OnlyOnceRegistered. <br />
     * (新規受け入れ可能フラグ)NEW_ACCEPTABLE_FLG: {NotNull, INTEGER(10), classification=Flg} <br />
     * general boolean classification for every flg-column
     * @param cdef The instance of classification definition (as ENUM type). (NullAllowed: if null, no condition)
     */
    def setNewAcceptableFlg_NotEqual_AsFlg(cdef: CDef.Flg): Unit = {
        doSetNewAcceptableFlg_NotEqual(cTNum(if (cdef != null) { cdef.code } else { null }, classOf[Integer]));
    }

    /**
     * NotEqual(&lt;&gt;). As True (1). And NullIgnored, OnlyOnceRegistered. <br />
     * Yes: means valid
     */
    def setNewAcceptableFlg_NotEqual_True(): Unit = {
        setNewAcceptableFlg_NotEqual_AsFlg(CDef.Flg.True);
    }

    /**
     * NotEqual(&lt;&gt;). As False (0). And NullIgnored, OnlyOnceRegistered. <br />
     * No: means invalid
     */
    def setNewAcceptableFlg_NotEqual_False(): Unit = {
        setNewAcceptableFlg_NotEqual_AsFlg(CDef.Flg.False);
    }

    protected def doSetNewAcceptableFlg_NotEqual(newAcceptableFlg: Integer): Unit = {
        regNewAcceptableFlg(CK_NES, newAcceptableFlg);
    }

    /**
     * InScope {in (1, 2)}. And NullIgnored, NullElementIgnored, SeveralRegistered. <br />
     * (新規受け入れ可能フラグ)NEW_ACCEPTABLE_FLG: {NotNull, INTEGER(10), classification=Flg}
     * @param newAcceptableFlgList The collection of newAcceptableFlg as inScope. (NullAllowed: if null (or empty), no condition)
     */
    def setNewAcceptableFlg_InScope(newAcceptableFlgList: Collection[Integer]): Unit = {
        doSetNewAcceptableFlg_InScope(newAcceptableFlgList);
    }

    /**
     * InScope {in (1, 2)}. As Flg. And NullIgnored, NullElementIgnored, SeveralRegistered. <br />
     * (新規受け入れ可能フラグ)NEW_ACCEPTABLE_FLG: {NotNull, INTEGER(10), classification=Flg} <br />
     * general boolean classification for every flg-column
     * @param cdefList The list of classification definition (as ENUM type). (NullAllowed: if null (or empty), no condition)
     */
    def setNewAcceptableFlg_InScope_AsFlg(cdefList: Collection[CDef.Flg]): Unit = {
        doSetNewAcceptableFlg_InScope(cTNumL(cdefList, classOf[Integer]));
    }

    protected def doSetNewAcceptableFlg_InScope(newAcceptableFlgList: Collection[Integer]): Unit = {
        regINS(CK_INS, cTL(newAcceptableFlgList), getCValueNewAcceptableFlg(), "NEW_ACCEPTABLE_FLG");
    }

    /**
     * NotInScope {not in (1, 2)}. And NullIgnored, NullElementIgnored, SeveralRegistered. <br />
     * (新規受け入れ可能フラグ)NEW_ACCEPTABLE_FLG: {NotNull, INTEGER(10), classification=Flg}
     * @param newAcceptableFlgList The collection of newAcceptableFlg as notInScope. (NullAllowed: if null (or empty), no condition)
     */
    def setNewAcceptableFlg_NotInScope(newAcceptableFlgList: Collection[Integer]): Unit = {
        doSetNewAcceptableFlg_NotInScope(newAcceptableFlgList);
    }

    /**
     * NotInScope {not in (1, 2)}. As Flg. And NullIgnored, NullElementIgnored, SeveralRegistered. <br />
     * (新規受け入れ可能フラグ)NEW_ACCEPTABLE_FLG: {NotNull, INTEGER(10), classification=Flg} <br />
     * general boolean classification for every flg-column
     * @param cdefList The list of classification definition (as ENUM type). (NullAllowed: if null (or empty), no condition)
     */
    def setNewAcceptableFlg_NotInScope_AsFlg(cdefList: Collection[CDef.Flg]): Unit = {
        doSetNewAcceptableFlg_NotInScope(cTNumL(cdefList, classOf[Integer]));
    }

    protected def doSetNewAcceptableFlg_NotInScope(newAcceptableFlgList: Collection[Integer]): Unit = {
        regINS(CK_NINS, cTL(newAcceptableFlgList), getCValueNewAcceptableFlg(), "NEW_ACCEPTABLE_FLG");
    }

    protected def regNewAcceptableFlg(ky: ConditionKey, vl: Object): Unit = { regQ(ky, vl, getCValueNewAcceptableFlg(), "NEW_ACCEPTABLE_FLG"); }
    protected def getCValueNewAcceptableFlg(): ConditionValue;

    /**
     * Equal(=). And NullOrEmptyIgnored, OnlyOnceRegistered. <br />
     * (説明)DESCRIPTION: {NotNull, VARCHAR(200)}
     * @param description The value of description as equal. (NullAllowed: if null (or empty), no condition)
     */
     def setDescription_Equal(description: String): Unit = {
        doSetDescription_Equal(fRES(description));
    }

    protected def doSetDescription_Equal(description: String): Unit = {
        regDescription(CK_EQ, description);
    }

    protected def regDescription(ky: ConditionKey, vl: Object): Unit = { regQ(ky, vl, getCValueDescription(), "DESCRIPTION"); }
    protected def getCValueDescription(): ConditionValue;
    
    /**
     * Equal(=). And NullIgnored, OnlyOnceRegistered. <br />
     * (表示順)DISPLAY_ORDER: {UQ, NotNull, INTEGER(10)}
     * @param displayOrder The value of displayOrder as equal. (NullAllowed: if null, no condition)
     */
     def setDisplayOrder_Equal(displayOrder: Integer): Unit = {
        doSetDisplayOrder_Equal(displayOrder);
    }

    protected def doSetDisplayOrder_Equal(displayOrder: Integer): Unit = {
        regDisplayOrder(CK_EQ, displayOrder);
    }

    /**
     * RangeOf with various options. (versatile) <br />
     * {(default) minNumber &lt;= column &lt;= maxNumber} <br />
     * And NullIgnored, OnlyOnceRegistered. <br />
     * (表示順)DISPLAY_ORDER: {UQ, NotNull, INTEGER(10)}
     * @param minNumber The min number of displayOrder. (NullAllowed: if null, no from-condition)
     * @param maxNumber The max number of displayOrder. (NullAllowed: if null, no to-condition)
     * @param rangeOfOption The option of range-of. (NotNull)
     */
    def setDisplayOrder_RangeOf(minNumber: Integer, maxNumber: Integer, rangeOfOption: RangeOfOption): Unit = {
        regROO(minNumber, maxNumber, getCValueDisplayOrder(), "DISPLAY_ORDER", rangeOfOption);
    }

    protected def regDisplayOrder(ky: ConditionKey, vl: Object): Unit = { regQ(ky, vl, getCValueDisplayOrder(), "DISPLAY_ORDER"); }
    protected def getCValueDisplayOrder(): ConditionValue;

    // ===================================================================================
    //                                                                     ScalarCondition
    //                                                                     ===============
    /**
     * Prepare ScalarCondition as equal. <br />
     * {where FOO = (select max(BAR) from ...)
     * <pre>
     * cb.query().<span style="color: #DD4747">scalar_Equal()</span>.max(new SubQuery&lt;ServiceRankCB&gt;() {
     *     public void query(ServiceRankCB subCB) {
     *         subCB.specify().setXxx... <span style="color: #3F7E5E">// derived column for function</span>
     *         subCB.query().setYyy...
     *     }
     * });
     * </pre>
     * @return The object to set up a function. (NotNull)
     */
    def scalar_Equal(): HpSSQFunction[ServiceRankCB] = {
        return xcreateSSQFunction(CK_EQ.getOperand(), classOf[ServiceRankCB]);
    }

    /**
     * Prepare ScalarCondition as equal. <br />
     * {where FOO &lt;&gt; (select max(BAR) from ...)
     * <pre>
     * cb.query().<span style="color: #DD4747">scalar_NotEqual()</span>.max(new SubQuery&lt;ServiceRankCB&gt;() {
     *     public void query(ServiceRankCB subCB) {
     *         subCB.specify().setXxx... <span style="color: #3F7E5E">// derived column for function</span>
     *         subCB.query().setYyy...
     *     }
     * });
     * </pre>
     * @return The object to set up a function. (NotNull)
     */
    def scalar_NotEqual(): HpSSQFunction[ServiceRankCB] = {
        return xcreateSSQFunction(CK_NES.getOperand(), classOf[ServiceRankCB]);
    }

    /**
     * Prepare ScalarCondition as greaterThan. <br />
     * {where FOO &gt; (select max(BAR) from ...)
     * <pre>
     * cb.query().<span style="color: #DD4747">scalar_GreaterThan()</span>.max(new SubQuery&lt;ServiceRankCB&gt;() {
     *     public void query(ServiceRankCB subCB) {
     *         subCB.specify().setFoo... <span style="color: #3F7E5E">// derived column for function</span>
     *         subCB.query().setBar...
     *     }
     * });
     * </pre>
     * @return The object to set up a function. (NotNull)
     */
    def scalar_GreaterThan(): HpSSQFunction[ServiceRankCB] = {
        return xcreateSSQFunction(CK_GT.getOperand(), classOf[ServiceRankCB]);
    }

    /**
     * Prepare ScalarCondition as lessThan. <br />
     * {where FOO &lt; (select max(BAR) from ...)
     * <pre>
     * cb.query().<span style="color: #DD4747">scalar_LessThan()</span>.max(new SubQuery&lt;ServiceRankCB&gt;() {
     *     public void query(ServiceRankCB subCB) {
     *         subCB.specify().setFoo... <span style="color: #3F7E5E">// derived column for function</span>
     *         subCB.query().setBar...
     *     }
     * });
     * </pre>
     * @return The object to set up a function. (NotNull)
     */
    def scalar_LessThan(): HpSSQFunction[ServiceRankCB] = {
        return xcreateSSQFunction(CK_LT.getOperand(), classOf[ServiceRankCB]);
    }

    /**
     * Prepare ScalarCondition as greaterEqual. <br />
     * {where FOO &gt;= (select max(BAR) from ...)
     * <pre>
     * cb.query().<span style="color: #DD4747">scalar_GreaterEqual()</span>.max(new SubQuery&lt;ServiceRankCB&gt;() {
     *     public void query(ServiceRankCB subCB) {
     *         subCB.specify().setFoo... <span style="color: #3F7E5E">// derived column for function</span>
     *         subCB.query().setBar...
     *     }
     * });
     * </pre>
     * @return The object to set up a function. (NotNull)
     */
    def scalar_GreaterEqual(): HpSSQFunction[ServiceRankCB] = {
        return xcreateSSQFunction(CK_GE.getOperand(), classOf[ServiceRankCB]);
    }

    /**
     * Prepare ScalarCondition as lessEqual. <br />
     * {where FOO &lt;= (select max(BAR) from ...)
     * <pre>
     * cb.query().<span style="color: #DD4747">scalar_LessEqual()</span>.max(new SubQuery&lt;ServiceRankCB&gt;() {
     *     public void query(ServiceRankCB subCB) {
     *         subCB.specify().setFoo... <span style="color: #3F7E5E">// derived column for function</span>
     *         subCB.query().setBar...
     *     }
     * });
     * </pre>
     * @return The object to set up a function. (NotNull)
     */
    def scalar_LessEqual(): HpSSQFunction[ServiceRankCB] = {
        return xcreateSSQFunction(CK_LE.getOperand(), classOf[ServiceRankCB]);
    }

    override protected def xscalarCondition[CB <: ConditionBean](fn: String, sq: SubQuery[CB], rd: String, op: HpSSQOption[CB]): Unit = {
        assertObjectNotNull("subQuery", sq);
        val cb: ServiceRankCB = xcreateScalarConditionCB(); sq.query(cb.asInstanceOf[CB]);
        val pp: String = keepScalarCondition(cb.query()); // for saving query-value
        op.setPartitionByCBean(xcreateScalarConditionPartitionByCB().asInstanceOf[CB]); // for using partition-by
        registerScalarCondition(fn, cb.query(), pp, rd, op);
    }
    def keepScalarCondition(sq: ServiceRankCQ): String;

    protected def xcreateScalarConditionCB(): ServiceRankCB = {
        val cb: ServiceRankCB = newMyCB(); cb.xsetupForScalarCondition(this); return cb;
    }

    protected def xcreateScalarConditionPartitionByCB(): ServiceRankCB = {
        val cb: ServiceRankCB = newMyCB(); cb.xsetupForScalarConditionPartitionBy(this); return cb;
    }

    // ===================================================================================
    //                                                                       MyselfDerived
    //                                                                       =============
    def xsmyselfDerive(fn: String, sq: SubQuery[ServiceRankCB], al: String, op: DerivedReferrerOption): Unit = {
        assertObjectNotNull("subQuery", sq);
        val cb: ServiceRankCB = new ServiceRankCB(); cb.xsetupForDerivedReferrer(this);
        try { lock(); sq.query(cb); } finally { unlock(); }
        val pp: String = keepSpecifyMyselfDerived(cb.query());
        val pk: String = "SERVICE_RANK_CODE";
        registerSpecifyMyselfDerived(fn, cb.query(), pk, pk, pp, "myselfDerived", al, op);
    }
    def keepSpecifyMyselfDerived(sq: ServiceRankCQ): String;

    /**
     * Prepare for (Query)MyselfDerived (correlated sub-query).
     * @return The object to set up a function for myself table. (NotNull)
     */
    def myselfDerived(): HpQDRFunction[ServiceRankCB] = {
        return xcreateQDRFunctionMyselfDerived(classOf[ServiceRankCB]);
    }
    override protected def xqderiveMyselfDerived[CB <: ConditionBean](fn: String, sq: SubQuery[CB], rd: String, vl: Object, op: DerivedReferrerOption): Unit = {
        assertObjectNotNull("subQuery", sq);
        val cb: ServiceRankCB = new ServiceRankCB(); cb.xsetupForDerivedReferrer(this); sq.query(cb.asInstanceOf[CB]);
        val pk: String = "SERVICE_RANK_CODE";
        val sqpp: String = keepQueryMyselfDerived(cb.query()); // for saving query-value.
        val prpp: String = keepQueryMyselfDerivedParameter(vl);
        registerQueryMyselfDerived(fn, cb.query(), pk, pk, sqpp, "myselfDerived", rd, vl, prpp, op);
    }
    def keepQueryMyselfDerived(sq: ServiceRankCQ): String;
    def keepQueryMyselfDerivedParameter(vl: Object): String;

    // ===================================================================================
    //                                                                        MyselfExists
    //                                                                        ============
    /**
     * Prepare for MyselfExists (correlated sub-query).
     * @param subQuery The implementation of sub-query. (NotNull)
     */
    def myselfExists(subQuery: (ServiceRankCB) => Unit): Unit = {
        assertObjectNotNull("subQuery", subQuery);
        val cb: ServiceRankCB = new ServiceRankCB(); cb.xsetupForMyselfExists(this);
        try { lock(); subQuery(cb); } finally { unlock(); }
        val pp: String = keepMyselfExists(cb.query());
        registerMyselfExists(cb.query(), pp);
    }
    def keepMyselfExists(sq: ServiceRankCQ): String;

    // ===================================================================================
    //                                                                       MyselfInScope
    //                                                                       =============
    /**
     * Prepare for MyselfInScope (sub-query).
     * @param subQuery The implementation of sub-query. (NotNull)
     */
    def myselfInScope(subQuery: (ServiceRankCB) => Unit): Unit = {
        assertObjectNotNull("subQuery", subQuery);
        val cb: ServiceRankCB = new ServiceRankCB(); cb.xsetupForMyselfInScope(this);
        try { lock(); subQuery(cb); } finally { unlock(); }
        val pp: String = keepMyselfInScope(cb.query());
        registerMyselfInScope(cb.query(), pp);
    }
    def keepMyselfInScope(sq: ServiceRankCQ): String;

    // ===================================================================================
    //                                                                       Very Internal
    //                                                                       =============
    protected def newMyCB(): ServiceRankCB = {
        return new ServiceRankCB();
    }
    // very internal (for suppressing warn about 'Not Use Import')
    protected def xabCQ(): String = { return classOf[ServiceRankCQ].getName(); }
    protected def xabLSO(): String = { return classOf[LikeSearchOption].getName(); }
    protected def xabSSQS(): String = { return classOf[HpSSQSetupper[_]].getName(); }
}
