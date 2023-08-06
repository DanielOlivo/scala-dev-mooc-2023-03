package module4.homework.services

import zio.Has
import zio.Task
import module4.homework.dao.entity.User
import module4.homework.dao.entity.Role
import module4.homework.dao.repository.UserRepository
import zio.ZIO
import zio.RIO
import module4.homework.dao.entity.UserToRole
import zio.ZLayer
import zio.macros.accessible
import module4.homework.dao.entity.RoleCode
import module4.homework.dao.entity.UserId
import module4.phoneBook.db

@accessible
object UserService{
    type UserService = Has[Service]

    trait Service{
        def listUsers(): RIO[db.DataSource, List[User]]
        def listUsersDTO(): RIO[db.DataSource, List[UserDTO]]
        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO]
        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource, List[UserDTO]]
    }

    class Impl(userRepo: UserRepository.Service) extends Service{
        val dc = db.Ctx
        import dc._

        def listUsers(): RIO[db.DataSource, List[User]] =
            userRepo.list()

        def userRoles(user : User): RIO[db.DataSource, List[Role]] =
            userRepo.userRoles(user.typedId)

        def userToDTO(user: User): RIO[db.DataSource, UserDTO] =
            for {
                roles <- userRepo.userRoles(user.typedId)
            } yield UserDTO(user, roles.toSet)

        def listUsersDTO(): RIO[db.DataSource,List[UserDTO]] = 
            for {
                users <- userRepo.list()
                list <- ZIO.succeed(users.map(userToDTO))
                dtos <- ZIO.collectAll {list}
            } yield dtos

        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO] = 
            //???
            for{
                _ <- userRepo.insertRoleToUser(roleCode, user.typedId)
                roles <- userRepo.userRoles(user.typedId)
                dto <- ZIO.succeed(UserDTO(user, roles.toSet))
            } yield dto
        
        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource,List[UserDTO]] = 
            for{
                users <- userRepo.listUsersWithRole(roleCode)
                list <- ZIO.succeed( users.map(userToDTO) )
                dtos <- ZIO.collectAll {list}
            } yield dtos
        
    }

    val live: ZLayer[UserRepository.UserRepository, Nothing, UserService] = 
        ZLayer.fromService(repo => new Impl(repo))
}

case class UserDTO(user: User, roles: Set[Role])