package com.example.dbflute.scala.handson

import com.atomikos.icatch.imp.SysExceptionTestJUnit
import com.example.dbflute.scala.dbflute.allcommon.{CDef, ScrLikeSearchOption, DBFlutist}
import com.example.dbflute.scala.dbflute.exentity.Member
import com.example.dbflute.scala.unit.UnitContainerFunSuite
import org.joda.time.LocalDate
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * @author iwamatsu0430
 */
@RunWith(classOf[JUnitRunner])
class HandsOn03Test extends UnitContainerFunSuite {

  test("handson_03") {
    /**
     * テストを実行する
     * @param mark marked用文字列
     * @param func テスト関数オブジェクト, markedHereを呼ぶ必要がある
     */
    def test_3(mark: String, func: String => Unit) = {
      func.apply(mark)
      assertMarked(mark)
    }

    //
    // 3-1
    // 会員名称がSで始まる1968年1月1日以前に生まれた会員を検索
    // 会員ステータスも取得する
    // 生年月日の昇順で並べる
    // 会員が1968/01/01以前であることをアサート
    //
    test_3("3_1", mark => {
      val targetDate: LocalDate = new LocalDate(1968, 1, 1)
      DBFlutist.memberBhv.selectList(cb => {
        cb.setupSelect_MemberStatus
        cb.query.setMemberName_PrefixSearch("S")
        cb.query.setBirthdate_LessThan(targetDate)
        cb.query.addOrderBy_Birthdate_Asc
      }).foreach(member => {
        println(s"===> $member")
        member.birthdate.foreach(birthdate => {
          markHere(mark)
          birthdate.isBefore(targetDate)
        })
      })
    })

    //
    // 3-2
    // 会員ステータスと会員セキュリティ情報も取得して会員を検索
    // 若い順で並べる。生年月日がない人は会員IDの昇順で並ぶようにする
    // 会員ステータスと会員セキュリティ情報が存在することをアサート
    //
    test_3("3_2", mark => {
      DBFlutist.memberBhv.selectList(cb => {
        cb.setupSelect_MemberStatus
        cb.setupSelect_MemberSecurityAsOne
        cb.query.addOrderBy_Birthdate_Desc
        cb.query.addOrderBy_MemberId_Asc
      }).foreach(member => {
        println(s"===> $member")
        assertNotNull(member.memberServiceAsOne)
        assertNotNull(member.memberSecurityAsOne)
        markHere(mark)
      })
    })

    //
    // 3-3
    // 会員セキュリティ情報のリマインダ質問で2という文字が含まれている会員を検索
    // 会員セキュリティ情報のデータ自体は要らない
    // リマインダ質問に2が含まれていることをアサート
    // アサートするために別途検索処理を入れても誰も文句は言わない
    //
    test_3("3_3", mark => {
      val targetStr: String = "2"
      DBFlutist.memberBhv.selectList(cb => {
        cb.query.queryMemberSecurityAsOne.setReminderQuestion_LikeSearch(targetStr)(op => op.likeContain)
      }).foreach(member => {
        println(s"===> $member")
        DBFlutist.memberSecurityBhv.selectEntity(cb => cb.query.setMemberId_Equal(member.memberId)).foreach(security => {
          assert(security.reminderQuestion.contains(targetStr))
          markHere(mark)
        })
      })
    })

    //
    // 3-4
    // 会員ステータスの表示順カラムで会員を並べて検索
    // 会員ステータスの "表示順" カラムの昇順で並べる
    // 会員ステータスのデータ自体は要らない
    // その次には、会員の会員IDの降順で並べる
    // 会員ステータスのデータが取れていないことをアサート
    // 会員が会員ステータスごとに固まって並んでいることをアサート
    //
    test_3("3_4", mark => {
      val members: List[Member] = DBFlutist.memberBhv.selectList(cb => {
        cb.query.queryMemberStatus.addOrderBy_DisplayOrder_Asc
        cb.query.addOrderBy_MemberId_Desc
      })

      // 会員ステータスのデータが取れていないことをアサート
      members.foreach(member => {
        assertEquals(None, member.memberStatus)
        markHere(mark)
      })

      // 会員が会員ステータスごとに固まって並んでいることをアサート
      def checkBlock(src: List[Member], statusCodes: List[CDef.MemberStatus] = List()): Boolean = (src, statusCodes) match {
        case (srcHead :: srcInit, Nil) => checkBlock(srcInit, srcHead.memberStatusCode :: statusCodes)
        case (srcHead :: srcInit, statusCodesHead :: statusCodesInit) if srcHead.memberStatusCode == statusCodesHead => checkBlock(srcInit, statusCodes)
        case (srcHead :: srcInit, statusCodesHead :: statusCodesInit) => statusCodesInit.contains(srcHead.memberStatusCode) match {
          case false => checkBlock(srcInit, srcHead.memberStatusCode :: statusCodes)
          case true  => false
        }
        case (Nil, codes) => true
      }

      assert(checkBlock(members))
    })
  }
}
