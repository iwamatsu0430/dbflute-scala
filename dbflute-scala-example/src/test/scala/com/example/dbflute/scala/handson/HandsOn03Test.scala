package com.example.dbflute.scala.handson

import com.atomikos.icatch.imp.SysExceptionTestJUnit
import com.example.dbflute.scala.dbflute.allcommon.{ScrLikeSearchOption, DBFlutist}
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
  }
}
