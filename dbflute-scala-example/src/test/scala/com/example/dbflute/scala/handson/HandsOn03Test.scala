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

  test("hands-on03") {
    //
    // 3-1
    // 会員名称がSで始まる1968年1月1日以前に生まれた会員を検索
    // 会員ステータスも取得する
    // 生年月日の昇順で並べる
    // 会員が1968/01/01以前であることをアサート
    //
    {
      () => {
        // Arrange
        val mark: String = "3_1"
        val targetDate: LocalDate = new LocalDate(1968, 1, 1)

        // Act
        DBFlutist.memberBhv.selectList(cb => {
          cb.setupSelect_MemberStatus
          cb.query.setMemberName_PrefixSearch("S")
          cb.query.setBirthdate_LessThan(targetDate)
          cb.query.addOrderBy_Birthdate_Asc
        }).foreach(member => {
          // Assert
          member.birthdate.foreach(birthdate => {
            markHere(mark)
            birthdate.isBefore(targetDate)
          })
        })
        assertMarked(mark)
      }
    }.apply

    //
    // 3-2
    // 会員ステータスと会員セキュリティ情報も取得して会員を検索
    // 若い順で並べる。生年月日がない人は会員IDの昇順で並ぶようにする
    // 会員ステータスと会員セキュリティ情報が存在することをアサート
    //
    {
      () => {
        // Arrange
        val mark: String = "3_2"

        // Act
        DBFlutist.memberBhv.selectList(cb => {
          cb.setupSelect_MemberStatus
          cb.setupSelect_MemberSecurityAsOne
          cb.query.addOrderBy_Birthdate_Desc
          cb.query.addOrderBy_MemberId_Asc
        }).foreach(member => {
          // Assert
          assertNotNull(member.memberServiceAsOne)
          assertNotNull(member.memberSecurityAsOne)
          markHere(mark)
        })
        assertMarked(mark)
      }
    }.apply

    //
    // 3-3
    // 会員セキュリティ情報のリマインダ質問で2という文字が含まれている会員を検索
    // 会員セキュリティ情報のデータ自体は要らない
    // リマインダ質問に2が含まれていることをアサート
    // アサートするために別途検索処理を入れても誰も文句は言わない
    //
    {
      () => {
        // Arrange
        val mark: String = "3_3"
        val targetStr: String = "2"

        // Act
        DBFlutist.memberBhv.selectList(cb => {
          cb.query.queryMemberSecurityAsOne.setReminderQuestion_LikeSearch(targetStr)(op => op.likeContain)
        }).foreach(member => {
          // Assert
          DBFlutist.memberSecurityBhv.selectEntity(cb => cb.query.setMemberId_Equal(member.memberId)).foreach(security => {
            assert(security.reminderQuestion.contains(targetStr))
            markHere(mark)
          })
        })
        assertMarked(mark)
      }
    }.apply

    //
    // 3-4
    // 会員ステータスの表示順カラムで会員を並べて検索
    // 会員ステータスの "表示順" カラムの昇順で並べる
    // 会員ステータスのデータ自体は要らない
    // その次には、会員の会員IDの降順で並べる
    // 会員ステータスのデータが取れていないことをアサート
    // 会員が会員ステータスごとに固まって並んでいることをアサート
    //
    {
      () => {
        // Act
        val members: List[Member] = DBFlutist.memberBhv.selectList(cb => {
          cb.query.queryMemberStatus.addOrderBy_DisplayOrder_Asc
          cb.query.addOrderBy_MemberId_Desc
        })

        // Assert
        assert(members.size > 0)
        members.foreach(member => assertEquals(None, member.memberStatus)) // 会員ステータスのデータが取れていないことをアサート
        assertBlock(members.map(_.memberStatusCode)) // 会員が会員ステータスごとに固まって並んでいることをアサート
      }
    }.apply

    //
    // 3-5
    // 生年月日が存在する会員の購入を検索
    // 会員名称と会員ステータス名称と商品名も一緒に取得(ログ出力)
    // 購入日時の降順、購入価格の降順、商品IDの昇順、会員IDの昇順で並べる
    // OrderBy がたくさん追加されていることをログで確認すること
    // 購入に紐づく会員の生年月日が存在することをアサート
    //
    {
      () => {
        // Arrange
        val mark: String = "3_5"

        // Act
        DBFlutist.purchaseBhv.selectList(cb => {
          cb.setupSelect_Member
          cb.setupSelect_Product
          cb.query.queryMember.setBirthdate_IsNotNull
          cb.query.addOrderBy_PurchaseDatetime_Desc
          cb.query.addOrderBy_PurchasePrice_Desc
          cb.query.queryProduct.addOrderBy_ProductId_Asc
          cb.query.queryMember.addOrderBy_MemberId_Asc
        }).foreach(purchase => {
          // Assert
          purchase.member.foreach(member => {
            assertNotNull(member.birthdate)
            markHere(mark)
          })
        })
        assertMarked(mark)
      }
    }.apply

    /**
     * 要素が固まって並んでいる事をアサート
     * @param origin 対象リスト
     */
    def assertBlock[A](origin: List[A]): Unit = {
      def inner(src: List[A], dst: List[A]): Unit = (src, dst) match {
        case (srcHead :: srcInit, Nil) => inner(srcInit, srcHead :: dst)
        case (srcHead :: srcInit, dstHead :: dstInit) if srcHead == dstHead => inner(srcInit, dst)
        case (srcHead :: srcInit, dstHead :: dstInit) => dstInit.contains(srcHead) match {
          case false => inner(srcInit, srcHead :: dst)
          case true  => fail() // 固まって並んでいない
        }
        case (Nil, codes) => // 固まって並んでいる
      }
      inner(origin, List())
    }
  }
}
